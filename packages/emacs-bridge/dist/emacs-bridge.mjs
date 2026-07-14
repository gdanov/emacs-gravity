
import { createRequire as __createRequire } from "module";
import { fileURLToPath as __fileURLToPath } from "url";
import { dirname as __dirnameFn } from "path";
const require = __createRequire(import.meta.url);
const __filename = __fileURLToPath(import.meta.url);
const __dirname = __dirnameFn(__filename);

var __create = Object.create;
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getProtoOf = Object.getPrototypeOf;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __require = /* @__PURE__ */ ((x) => typeof require !== "undefined" ? require : typeof Proxy !== "undefined" ? new Proxy(x, {
  get: (a, b) => (typeof require !== "undefined" ? require : a)[b]
}) : x)(function(x) {
  if (typeof require !== "undefined") return require.apply(this, arguments);
  throw Error('Dynamic require of "' + x + '" is not supported');
});
var __esm = (fn3, res) => function __init() {
  return fn3 && (res = (0, fn3[__getOwnPropNames(fn3)[0]])(fn3 = 0)), res;
};
var __commonJS = (cb, mod) => function __require2() {
  return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
};
var __export = (target, all3) => {
  for (var name in all3)
    __defProp(target, name, { get: all3[name], enumerable: true });
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
  // If the importer is in node compatibility mode or this is not an ESM
  // file that has been converted to a CommonJS file using a Babel-
  // compatible transform (i.e. "__esModule" has not been set), then set
  // "default" to the CommonJS "module.exports" for node compatibility.
  isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
  mod
));

// node_modules/bonjour-service/dist/lib/utils/dns-equal.js
var require_dns_equal = __commonJS({
  "node_modules/bonjour-service/dist/lib/utils/dns-equal.js"(exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.default = dnsEqual;
    var capitalLetterRegex = /[A-Z]/g;
    function toLowerCase(input) {
      return input.toLowerCase();
    }
    function dnsEqual(a, b) {
      const aFormatted = a.replace(capitalLetterRegex, toLowerCase);
      const bFormatted = b.replace(capitalLetterRegex, toLowerCase);
      return aFormatted === bFormatted;
    }
  }
});

// node_modules/bonjour-service/dist/lib/dns-txt.js
var require_dns_txt = __commonJS({
  "node_modules/bonjour-service/dist/lib/dns-txt.js"(exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.DnsTxt = void 0;
    var DnsTxt = class {
      constructor(opts = {}) {
        this.binary = opts ? opts.binary : false;
      }
      encode(data = {}) {
        return Object.entries(data).map(([key, value]) => {
          let item = `${key}=${value}`;
          return Buffer.from(item);
        });
      }
      decode(buffer) {
        var data = {};
        try {
          let format3 = buffer.toString();
          let parts = format3.split(/=(.+)/);
          let key = parts[0];
          let value = parts[1];
          data[key] = value;
        } catch (_) {
        }
        return data;
      }
      decodeAll(buffer) {
        return buffer.filter((i) => i.length > 1).map((i) => this.decode(i)).reduce((prev, curr) => {
          var obj = prev;
          let [key] = Object.keys(curr);
          let [value] = Object.values(curr);
          obj[key] = value;
          return obj;
        }, {});
      }
    };
    exports.DnsTxt = DnsTxt;
    exports.default = DnsTxt;
  }
});

// node_modules/bonjour-service/dist/lib/service-types.js
var require_service_types = __commonJS({
  "node_modules/bonjour-service/dist/lib/service-types.js"(exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.toType = exports.toString = void 0;
    var Prefix = (name) => {
      return "_" + name;
    };
    var AllowedProp = (key) => {
      let keys = ["name", "protocol", "subtype"];
      return keys.includes(key);
    };
    var toString = (data) => {
      let formatted = {
        name: data.name,
        protocol: data.protocol,
        subtype: data.subtype
      };
      let entries = Object.entries(formatted);
      return entries.filter(([key, val]) => AllowedProp(key) && val !== void 0).reduce((prev, [key, val]) => {
        switch (typeof val) {
          case "object":
            val.map((i) => prev.push(Prefix(i)));
            break;
          default:
            prev.push(Prefix(val));
            break;
        }
        return prev;
      }, []).join(".");
    };
    exports.toString = toString;
    var toType = (string2) => {
      let parts = string2.split(".");
      let subtype;
      for (let i in parts) {
        if (parts[i][0] !== "_")
          continue;
        parts[i] = parts[i].slice(1);
      }
      if (parts.includes("sub")) {
        subtype = parts.shift();
        parts.shift();
      }
      return {
        name: parts.shift(),
        protocol: parts.shift() || null,
        subtype
      };
    };
    exports.toType = toType;
  }
});

// node_modules/bonjour-service/dist/lib/service.js
var require_service = __commonJS({
  "node_modules/bonjour-service/dist/lib/service.js"(exports) {
    "use strict";
    var __importDefault = exports && exports.__importDefault || function(mod) {
      return mod && mod.__esModule ? mod : { "default": mod };
    };
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Service = void 0;
    var os_1 = __importDefault(__require("os"));
    var dns_txt_1 = __importDefault(require_dns_txt());
    var events_1 = __require("events");
    var service_types_1 = require_service_types();
    var TLD = ".local";
    var Service3 = class extends events_1.EventEmitter {
      constructor(config) {
        super();
        this.probe = true;
        this.published = false;
        this.activated = false;
        this.destroyed = false;
        this.txtService = new dns_txt_1.default();
        if (!config.name)
          throw new Error("ServiceConfig requires `name` property to be set");
        if (!config.type)
          throw new Error("ServiceConfig requires `type` property to be set");
        if (!config.port)
          throw new Error("ServiceConfig requires `port` property to be set");
        this.name = config.name.split(".").join("-");
        this.protocol = config.protocol || "tcp";
        this.type = (0, service_types_1.toString)({ name: config.type, protocol: this.protocol });
        this.port = config.port;
        this.host = config.host || os_1.default.hostname();
        this.fqdn = `${this.name}.${this.type}${TLD}`;
        this.txt = config.txt;
        this.subtypes = config.subtypes;
        this.disableIPv6 = !!config.disableIPv6;
      }
      records() {
        var records = [this.RecordPTR(this), this.RecordSRV(this), this.RecordTXT(this)];
        for (let subtype of this.subtypes || []) {
          records.push(this.RecordSubtypePTR(this, subtype));
        }
        let ifaces = Object.values(os_1.default.networkInterfaces());
        for (let iface of ifaces) {
          let addrs = iface;
          for (let addr of addrs) {
            if (addr.internal || addr.mac === "00:00:00:00:00:00")
              continue;
            switch (addr.family) {
              case "IPv4":
                records.push(this.RecordA(this, addr.address));
                break;
              case "IPv6":
                if (this.disableIPv6)
                  break;
                records.push(this.RecordAAAA(this, addr.address));
                break;
            }
          }
        }
        return records;
      }
      RecordPTR(service3) {
        return {
          name: `${service3.type}${TLD}`,
          type: "PTR",
          ttl: 28800,
          data: service3.fqdn
        };
      }
      RecordSubtypePTR(service3, subtype) {
        return {
          name: `_${subtype}._sub.${service3.type}${TLD}`,
          type: "PTR",
          ttl: 28800,
          data: `${service3.name}.${service3.type}${TLD}`
        };
      }
      RecordSRV(service3) {
        return {
          name: service3.fqdn,
          type: "SRV",
          ttl: 120,
          data: {
            port: service3.port,
            target: service3.host
          }
        };
      }
      RecordTXT(service3) {
        return {
          name: service3.fqdn,
          type: "TXT",
          ttl: 4500,
          data: this.txtService.encode(service3.txt)
        };
      }
      RecordA(service3, ip) {
        return {
          name: service3.host,
          type: "A",
          ttl: 120,
          data: ip
        };
      }
      RecordAAAA(service3, ip) {
        return {
          name: service3.host,
          type: "AAAA",
          ttl: 120,
          data: ip
        };
      }
    };
    exports.Service = Service3;
    exports.default = Service3;
  }
});

// node_modules/bonjour-service/dist/lib/registry.js
var require_registry = __commonJS({
  "node_modules/bonjour-service/dist/lib/registry.js"(exports) {
    "use strict";
    var __importDefault = exports && exports.__importDefault || function(mod) {
      return mod && mod.__esModule ? mod : { "default": mod };
    };
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Registry = void 0;
    var dns_equal_1 = __importDefault(require_dns_equal());
    var service_1 = __importDefault(require_service());
    var REANNOUNCE_MAX_MS = 60 * 60 * 1e3;
    var REANNOUNCE_FACTOR = 3;
    var noop = function() {
    };
    var Registry = class {
      constructor(server) {
        this.services = [];
        this.server = server;
      }
      publish(config) {
        function start(service4, registry, opts) {
          if (service4.activated)
            return;
          service4.activated = true;
          registry.services.push(service4);
          if (!(service4 instanceof service_1.default))
            return;
          if (opts === null || opts === void 0 ? void 0 : opts.probe) {
            registry.probe(registry.server.mdns, service4, (exists) => {
              if (exists) {
                if (service4.stop !== void 0)
                  service4.stop();
                console.log(new Error("Service name is already in use on the network"));
                return;
              }
              registry.announce(registry.server, service4);
            });
          } else {
            registry.announce(registry.server, service4);
          }
        }
        function stop(service4, registry, callback3) {
          if (!callback3)
            callback3 = noop;
          if (!service4.activated)
            return process.nextTick(callback3);
          if (!(service4 instanceof service_1.default))
            return process.nextTick(callback3);
          registry.teardown(registry.server, service4, callback3);
          const index = registry.services.indexOf(service4);
          if (index !== -1)
            registry.services.splice(index, 1);
        }
        const service3 = new service_1.default(config);
        service3.start = start.bind(null, service3, this);
        service3.stop = stop.bind(null, service3, this);
        service3.start({ probe: config.probe !== false });
        return service3;
      }
      unpublishAll(callback3) {
        this.teardown(this.server, this.services, callback3);
        this.services = [];
      }
      destroy() {
        this.services.map((service3) => service3.destroyed = true);
      }
      probe(mdns, service3, callback3) {
        var sent = false;
        var retries = 0;
        var timer;
        const send = () => {
          if (!service3.activated || service3.destroyed)
            return;
          mdns.query(service3.fqdn, "ANY", function() {
            sent = true;
            timer = setTimeout(++retries < 3 ? send : done4, 250);
            timer.unref();
          });
        };
        const onresponse = (packet) => {
          if (!sent)
            return;
          if (packet.answers.some(matchRR) || packet.additionals.some(matchRR))
            done4(true);
        };
        const matchRR = (rr) => {
          return (0, dns_equal_1.default)(rr.name, service3.fqdn);
        };
        const done4 = (exists) => {
          mdns.removeListener("response", onresponse);
          clearTimeout(timer);
          callback3(!!exists);
        };
        mdns.on("response", onresponse);
        setTimeout(send, Math.random() * 250);
      }
      announce(server, service3) {
        var delay3 = 1e3;
        var packet = service3.records();
        server.register(packet);
        const broadcast = () => {
          if (!service3.activated || service3.destroyed)
            return;
          server.mdns.respond(packet, function() {
            if (!service3.published) {
              service3.activated = true;
              service3.published = true;
              service3.emit("up");
            }
            delay3 = delay3 * REANNOUNCE_FACTOR;
            if (delay3 < REANNOUNCE_MAX_MS && !service3.destroyed) {
              setTimeout(broadcast, delay3).unref();
            }
          });
        };
        broadcast();
      }
      teardown(server, services3, callback3) {
        if (!Array.isArray(services3))
          services3 = [services3];
        services3 = services3.filter((service3) => service3.activated);
        var records = services3.flatMap(function(service3) {
          service3.activated = false;
          var records2 = service3.records();
          records2.forEach((record) => {
            record.ttl = 0;
          });
          return records2;
        });
        if (records.length === 0)
          return callback3 && process.nextTick(callback3);
        server.unregister(records);
        server.mdns.respond(records, function() {
          services3.forEach(function(service3) {
            service3.published = false;
          });
          if (typeof callback3 === "function") {
            callback3.apply(null, arguments);
          }
        });
      }
    };
    exports.Registry = Registry;
    exports.default = Registry;
  }
});

// node_modules/dns-packet/types.js
var require_types = __commonJS({
  "node_modules/dns-packet/types.js"(exports) {
    "use strict";
    exports.toString = function(type) {
      switch (type) {
        case 1:
          return "A";
        case 10:
          return "NULL";
        case 28:
          return "AAAA";
        case 18:
          return "AFSDB";
        case 42:
          return "APL";
        case 257:
          return "CAA";
        case 60:
          return "CDNSKEY";
        case 59:
          return "CDS";
        case 37:
          return "CERT";
        case 5:
          return "CNAME";
        case 49:
          return "DHCID";
        case 32769:
          return "DLV";
        case 39:
          return "DNAME";
        case 48:
          return "DNSKEY";
        case 43:
          return "DS";
        case 55:
          return "HIP";
        case 13:
          return "HINFO";
        case 45:
          return "IPSECKEY";
        case 25:
          return "KEY";
        case 36:
          return "KX";
        case 29:
          return "LOC";
        case 15:
          return "MX";
        case 35:
          return "NAPTR";
        case 2:
          return "NS";
        case 47:
          return "NSEC";
        case 50:
          return "NSEC3";
        case 51:
          return "NSEC3PARAM";
        case 12:
          return "PTR";
        case 46:
          return "RRSIG";
        case 17:
          return "RP";
        case 24:
          return "SIG";
        case 6:
          return "SOA";
        case 99:
          return "SPF";
        case 33:
          return "SRV";
        case 44:
          return "SSHFP";
        case 32768:
          return "TA";
        case 249:
          return "TKEY";
        case 52:
          return "TLSA";
        case 250:
          return "TSIG";
        case 16:
          return "TXT";
        case 252:
          return "AXFR";
        case 251:
          return "IXFR";
        case 41:
          return "OPT";
        case 255:
          return "ANY";
      }
      return "UNKNOWN_" + type;
    };
    exports.toType = function(name) {
      switch (name.toUpperCase()) {
        case "A":
          return 1;
        case "NULL":
          return 10;
        case "AAAA":
          return 28;
        case "AFSDB":
          return 18;
        case "APL":
          return 42;
        case "CAA":
          return 257;
        case "CDNSKEY":
          return 60;
        case "CDS":
          return 59;
        case "CERT":
          return 37;
        case "CNAME":
          return 5;
        case "DHCID":
          return 49;
        case "DLV":
          return 32769;
        case "DNAME":
          return 39;
        case "DNSKEY":
          return 48;
        case "DS":
          return 43;
        case "HIP":
          return 55;
        case "HINFO":
          return 13;
        case "IPSECKEY":
          return 45;
        case "KEY":
          return 25;
        case "KX":
          return 36;
        case "LOC":
          return 29;
        case "MX":
          return 15;
        case "NAPTR":
          return 35;
        case "NS":
          return 2;
        case "NSEC":
          return 47;
        case "NSEC3":
          return 50;
        case "NSEC3PARAM":
          return 51;
        case "PTR":
          return 12;
        case "RRSIG":
          return 46;
        case "RP":
          return 17;
        case "SIG":
          return 24;
        case "SOA":
          return 6;
        case "SPF":
          return 99;
        case "SRV":
          return 33;
        case "SSHFP":
          return 44;
        case "TA":
          return 32768;
        case "TKEY":
          return 249;
        case "TLSA":
          return 52;
        case "TSIG":
          return 250;
        case "TXT":
          return 16;
        case "AXFR":
          return 252;
        case "IXFR":
          return 251;
        case "OPT":
          return 41;
        case "ANY":
          return 255;
        case "*":
          return 255;
      }
      if (name.toUpperCase().startsWith("UNKNOWN_")) return parseInt(name.slice(8));
      return 0;
    };
  }
});

// node_modules/dns-packet/rcodes.js
var require_rcodes = __commonJS({
  "node_modules/dns-packet/rcodes.js"(exports) {
    "use strict";
    exports.toString = function(rcode) {
      switch (rcode) {
        case 0:
          return "NOERROR";
        case 1:
          return "FORMERR";
        case 2:
          return "SERVFAIL";
        case 3:
          return "NXDOMAIN";
        case 4:
          return "NOTIMP";
        case 5:
          return "REFUSED";
        case 6:
          return "YXDOMAIN";
        case 7:
          return "YXRRSET";
        case 8:
          return "NXRRSET";
        case 9:
          return "NOTAUTH";
        case 10:
          return "NOTZONE";
        case 11:
          return "RCODE_11";
        case 12:
          return "RCODE_12";
        case 13:
          return "RCODE_13";
        case 14:
          return "RCODE_14";
        case 15:
          return "RCODE_15";
      }
      return "RCODE_" + rcode;
    };
    exports.toRcode = function(code) {
      switch (code.toUpperCase()) {
        case "NOERROR":
          return 0;
        case "FORMERR":
          return 1;
        case "SERVFAIL":
          return 2;
        case "NXDOMAIN":
          return 3;
        case "NOTIMP":
          return 4;
        case "REFUSED":
          return 5;
        case "YXDOMAIN":
          return 6;
        case "YXRRSET":
          return 7;
        case "NXRRSET":
          return 8;
        case "NOTAUTH":
          return 9;
        case "NOTZONE":
          return 10;
        case "RCODE_11":
          return 11;
        case "RCODE_12":
          return 12;
        case "RCODE_13":
          return 13;
        case "RCODE_14":
          return 14;
        case "RCODE_15":
          return 15;
      }
      return 0;
    };
  }
});

// node_modules/dns-packet/opcodes.js
var require_opcodes = __commonJS({
  "node_modules/dns-packet/opcodes.js"(exports) {
    "use strict";
    exports.toString = function(opcode) {
      switch (opcode) {
        case 0:
          return "QUERY";
        case 1:
          return "IQUERY";
        case 2:
          return "STATUS";
        case 3:
          return "OPCODE_3";
        case 4:
          return "NOTIFY";
        case 5:
          return "UPDATE";
        case 6:
          return "OPCODE_6";
        case 7:
          return "OPCODE_7";
        case 8:
          return "OPCODE_8";
        case 9:
          return "OPCODE_9";
        case 10:
          return "OPCODE_10";
        case 11:
          return "OPCODE_11";
        case 12:
          return "OPCODE_12";
        case 13:
          return "OPCODE_13";
        case 14:
          return "OPCODE_14";
        case 15:
          return "OPCODE_15";
      }
      return "OPCODE_" + opcode;
    };
    exports.toOpcode = function(code) {
      switch (code.toUpperCase()) {
        case "QUERY":
          return 0;
        case "IQUERY":
          return 1;
        case "STATUS":
          return 2;
        case "OPCODE_3":
          return 3;
        case "NOTIFY":
          return 4;
        case "UPDATE":
          return 5;
        case "OPCODE_6":
          return 6;
        case "OPCODE_7":
          return 7;
        case "OPCODE_8":
          return 8;
        case "OPCODE_9":
          return 9;
        case "OPCODE_10":
          return 10;
        case "OPCODE_11":
          return 11;
        case "OPCODE_12":
          return 12;
        case "OPCODE_13":
          return 13;
        case "OPCODE_14":
          return 14;
        case "OPCODE_15":
          return 15;
      }
      return 0;
    };
  }
});

// node_modules/dns-packet/classes.js
var require_classes = __commonJS({
  "node_modules/dns-packet/classes.js"(exports) {
    "use strict";
    exports.toString = function(klass) {
      switch (klass) {
        case 1:
          return "IN";
        case 2:
          return "CS";
        case 3:
          return "CH";
        case 4:
          return "HS";
        case 255:
          return "ANY";
      }
      return "UNKNOWN_" + klass;
    };
    exports.toClass = function(name) {
      switch (name.toUpperCase()) {
        case "IN":
          return 1;
        case "CS":
          return 2;
        case "CH":
          return 3;
        case "HS":
          return 4;
        case "ANY":
          return 255;
      }
      return 0;
    };
  }
});

// node_modules/dns-packet/optioncodes.js
var require_optioncodes = __commonJS({
  "node_modules/dns-packet/optioncodes.js"(exports) {
    "use strict";
    exports.toString = function(type) {
      switch (type) {
        // list at
        // https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-11
        case 1:
          return "LLQ";
        case 2:
          return "UL";
        case 3:
          return "NSID";
        case 5:
          return "DAU";
        case 6:
          return "DHU";
        case 7:
          return "N3U";
        case 8:
          return "CLIENT_SUBNET";
        case 9:
          return "EXPIRE";
        case 10:
          return "COOKIE";
        case 11:
          return "TCP_KEEPALIVE";
        case 12:
          return "PADDING";
        case 13:
          return "CHAIN";
        case 14:
          return "KEY_TAG";
        case 26946:
          return "DEVICEID";
      }
      if (type < 0) {
        return null;
      }
      return `OPTION_${type}`;
    };
    exports.toCode = function(name) {
      if (typeof name === "number") {
        return name;
      }
      if (!name) {
        return -1;
      }
      switch (name.toUpperCase()) {
        case "OPTION_0":
          return 0;
        case "LLQ":
          return 1;
        case "UL":
          return 2;
        case "NSID":
          return 3;
        case "OPTION_4":
          return 4;
        case "DAU":
          return 5;
        case "DHU":
          return 6;
        case "N3U":
          return 7;
        case "CLIENT_SUBNET":
          return 8;
        case "EXPIRE":
          return 9;
        case "COOKIE":
          return 10;
        case "TCP_KEEPALIVE":
          return 11;
        case "PADDING":
          return 12;
        case "CHAIN":
          return 13;
        case "KEY_TAG":
          return 14;
        case "DEVICEID":
          return 26946;
        case "OPTION_65535":
          return 65535;
      }
      const m = name.match(/_(\d+)$/);
      if (m) {
        return parseInt(m[1], 10);
      }
      return -1;
    };
  }
});

// node_modules/@leichtgewicht/ip-codec/index.cjs
var require_ip_codec = __commonJS({
  "node_modules/@leichtgewicht/ip-codec/index.cjs"(exports, module) {
    var ipCodec = (function(exports2) {
      "use strict";
      Object.defineProperty(exports2, "__esModule", {
        value: true
      });
      exports2.decode = decode;
      exports2.encode = encode;
      exports2.familyOf = familyOf;
      exports2.name = void 0;
      exports2.sizeOf = sizeOf;
      exports2.v6 = exports2.v4 = void 0;
      const v4Regex = /^(\d{1,3}\.){3,3}\d{1,3}$/;
      const v4Size = 4;
      const v6Regex = /^(::)?(((\d{1,3}\.){3}(\d{1,3}){1})?([0-9a-f]){0,4}:{0,2}){1,8}(::)?$/i;
      const v6Size = 16;
      const v4 = {
        name: "v4",
        size: v4Size,
        isFormat: (ip) => v4Regex.test(ip),
        encode(ip, buff, offset) {
          offset = ~~offset;
          buff = buff || new Uint8Array(offset + v4Size);
          const max2 = ip.length;
          let n = 0;
          for (let i = 0; i < max2; ) {
            const c = ip.charCodeAt(i++);
            if (c === 46) {
              buff[offset++] = n;
              n = 0;
            } else {
              n = n * 10 + (c - 48);
            }
          }
          buff[offset] = n;
          return buff;
        },
        decode(buff, offset) {
          offset = ~~offset;
          return `${buff[offset++]}.${buff[offset++]}.${buff[offset++]}.${buff[offset]}`;
        }
      };
      exports2.v4 = v4;
      const v6 = {
        name: "v6",
        size: v6Size,
        isFormat: (ip) => ip.length > 0 && v6Regex.test(ip),
        encode(ip, buff, offset) {
          offset = ~~offset;
          let end = offset + v6Size;
          let fill = -1;
          let hexN = 0;
          let decN = 0;
          let prevColon = true;
          let useDec = false;
          buff = buff || new Uint8Array(offset + v6Size);
          for (let i = 0; i < ip.length; i++) {
            let c = ip.charCodeAt(i);
            if (c === 58) {
              if (prevColon) {
                if (fill !== -1) {
                  if (offset < end) buff[offset] = 0;
                  if (offset < end - 1) buff[offset + 1] = 0;
                  offset += 2;
                } else if (offset < end) {
                  fill = offset;
                }
              } else {
                if (useDec === true) {
                  if (offset < end) buff[offset] = decN;
                  offset++;
                } else {
                  if (offset < end) buff[offset] = hexN >> 8;
                  if (offset < end - 1) buff[offset + 1] = hexN & 255;
                  offset += 2;
                }
                hexN = 0;
                decN = 0;
              }
              prevColon = true;
              useDec = false;
            } else if (c === 46) {
              if (offset < end) buff[offset] = decN;
              offset++;
              decN = 0;
              hexN = 0;
              prevColon = false;
              useDec = true;
            } else {
              prevColon = false;
              if (c >= 97) {
                c -= 87;
              } else if (c >= 65) {
                c -= 55;
              } else {
                c -= 48;
                decN = decN * 10 + c;
              }
              hexN = (hexN << 4) + c;
            }
          }
          if (prevColon === false) {
            if (useDec === true) {
              if (offset < end) buff[offset] = decN;
              offset++;
            } else {
              if (offset < end) buff[offset] = hexN >> 8;
              if (offset < end - 1) buff[offset + 1] = hexN & 255;
              offset += 2;
            }
          } else if (fill === 0) {
            if (offset < end) buff[offset] = 0;
            if (offset < end - 1) buff[offset + 1] = 0;
            offset += 2;
          } else if (fill !== -1) {
            offset += 2;
            for (let i = Math.min(offset - 1, end - 1); i >= fill + 2; i--) {
              buff[i] = buff[i - 2];
            }
            buff[fill] = 0;
            buff[fill + 1] = 0;
            fill = offset;
          }
          if (fill !== offset && fill !== -1) {
            if (offset > end - 2) {
              offset = end - 2;
            }
            while (end > fill) {
              buff[--end] = offset < end && offset > fill ? buff[--offset] : 0;
            }
          } else {
            while (offset < end) {
              buff[offset++] = 0;
            }
          }
          return buff;
        },
        decode(buff, offset) {
          offset = ~~offset;
          let result3 = "";
          for (let i = 0; i < v6Size; i += 2) {
            if (i !== 0) {
              result3 += ":";
            }
            result3 += (buff[offset + i] << 8 | buff[offset + i + 1]).toString(16);
          }
          return result3.replace(/(^|:)0(:0)*:0(:|$)/, "$1::$3").replace(/:{3,4}/, "::");
        }
      };
      exports2.v6 = v6;
      const name = "ip";
      exports2.name = name;
      function sizeOf(ip) {
        if (v4.isFormat(ip)) return v4.size;
        if (v6.isFormat(ip)) return v6.size;
        throw Error(`Invalid ip address: ${ip}`);
      }
      function familyOf(string2) {
        return sizeOf(string2) === v4.size ? 1 : 2;
      }
      function encode(ip, buff, offset) {
        offset = ~~offset;
        const size = sizeOf(ip);
        if (typeof buff === "function") {
          buff = buff(offset + size);
        }
        if (size === v4.size) {
          return v4.encode(ip, buff, offset);
        }
        return v6.encode(ip, buff, offset);
      }
      function decode(buff, offset, length) {
        offset = ~~offset;
        length = length || buff.length - offset;
        if (length === v4.size) {
          return v4.decode(buff, offset, length);
        }
        if (length === v6.size) {
          return v6.decode(buff, offset, length);
        }
        throw Error(`Invalid buffer size needs to be ${v4.size} for v4 or ${v6.size} for v6.`);
      }
      return "default" in exports2 ? exports2.default : exports2;
    })({});
    if (typeof define === "function" && define.amd) define([], function() {
      return ipCodec;
    });
    else if (typeof module === "object" && typeof exports === "object") module.exports = ipCodec;
  }
});

// node_modules/dns-packet/index.js
var require_dns_packet = __commonJS({
  "node_modules/dns-packet/index.js"(exports) {
    "use strict";
    var Buffer2 = __require("buffer").Buffer;
    var types = require_types();
    var rcodes = require_rcodes();
    var opcodes = require_opcodes();
    var classes = require_classes();
    var optioncodes = require_optioncodes();
    var ip = require_ip_codec();
    var QUERY_FLAG = 0;
    var RESPONSE_FLAG = 1 << 15;
    var FLUSH_MASK = 1 << 15;
    var NOT_FLUSH_MASK = ~FLUSH_MASK;
    var QU_MASK = 1 << 15;
    var NOT_QU_MASK = ~QU_MASK;
    var name = exports.name = {};
    name.encode = function(str, buf, offset, { mail = false } = {}) {
      if (!buf) buf = Buffer2.alloc(name.encodingLength(str));
      if (!offset) offset = 0;
      const oldOffset = offset;
      const n = str.replace(/^\.|\.$/gm, "");
      if (n.length) {
        let list = [];
        if (mail) {
          let localPart = "";
          n.split(".").forEach((label) => {
            if (label.endsWith("\\")) {
              localPart += (localPart.length ? "." : "") + label.slice(0, -1);
            } else {
              if (list.length === 0 && localPart.length) {
                list.push(localPart + "." + label);
              } else {
                list.push(label);
              }
            }
          });
        } else {
          list = n.split(".");
        }
        for (let i = 0; i < list.length; i++) {
          const len = buf.write(list[i], offset + 1);
          buf[offset] = len;
          offset += len + 1;
        }
      }
      buf[offset++] = 0;
      name.encode.bytes = offset - oldOffset;
      return buf;
    };
    name.encode.bytes = 0;
    name.decode = function(buf, offset, { mail = false } = {}) {
      if (!offset) offset = 0;
      const list = [];
      let oldOffset = offset;
      let totalLength = 0;
      let consumedBytes = 0;
      let jumped = false;
      while (true) {
        if (offset >= buf.length) {
          throw new Error("Cannot decode name (buffer overflow)");
        }
        const len = buf[offset++];
        consumedBytes += jumped ? 0 : 1;
        if (len === 0) {
          break;
        } else if ((len & 192) === 0) {
          if (offset + len > buf.length) {
            throw new Error("Cannot decode name (buffer overflow)");
          }
          totalLength += len + 1;
          if (totalLength > 254) {
            throw new Error("Cannot decode name (name too long)");
          }
          let label = buf.toString("utf-8", offset, offset + len);
          if (mail) {
            label = label.replace(/\./g, "\\.");
          }
          list.push(label);
          offset += len;
          consumedBytes += jumped ? 0 : len;
        } else if ((len & 192) === 192) {
          if (offset + 1 > buf.length) {
            throw new Error("Cannot decode name (buffer overflow)");
          }
          const jumpOffset = buf.readUInt16BE(offset - 1) - 49152;
          if (jumpOffset >= oldOffset) {
            throw new Error("Cannot decode name (bad pointer)");
          }
          offset = jumpOffset;
          oldOffset = jumpOffset;
          consumedBytes += jumped ? 0 : 1;
          jumped = true;
        } else {
          throw new Error("Cannot decode name (bad label)");
        }
      }
      name.decode.bytes = consumedBytes;
      return list.length === 0 ? "." : list.join(".");
    };
    name.decode.bytes = 0;
    name.encodingLength = function(n) {
      if (n === "." || n === "..") return 1;
      return Buffer2.byteLength(n.replace(/^\.|\.$/gm, "")) + 2;
    };
    var string2 = {};
    string2.encode = function(s, buf, offset) {
      if (!buf) buf = Buffer2.alloc(string2.encodingLength(s));
      if (!offset) offset = 0;
      const len = buf.write(s, offset + 1);
      buf[offset] = len;
      string2.encode.bytes = len + 1;
      return buf;
    };
    string2.encode.bytes = 0;
    string2.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const len = buf[offset];
      const s = buf.toString("utf-8", offset + 1, offset + 1 + len);
      string2.decode.bytes = len + 1;
      return s;
    };
    string2.decode.bytes = 0;
    string2.encodingLength = function(s) {
      return Buffer2.byteLength(s) + 1;
    };
    var header = {};
    header.encode = function(h, buf, offset) {
      if (!buf) buf = header.encodingLength(h);
      if (!offset) offset = 0;
      const flags = (h.flags || 0) & 32767;
      const type = h.type === "response" ? RESPONSE_FLAG : QUERY_FLAG;
      buf.writeUInt16BE(h.id || 0, offset);
      buf.writeUInt16BE(flags | type, offset + 2);
      buf.writeUInt16BE(h.questions.length, offset + 4);
      buf.writeUInt16BE(h.answers.length, offset + 6);
      buf.writeUInt16BE(h.authorities.length, offset + 8);
      buf.writeUInt16BE(h.additionals.length, offset + 10);
      return buf;
    };
    header.encode.bytes = 12;
    header.decode = function(buf, offset) {
      if (!offset) offset = 0;
      if (buf.length < 12) throw new Error("Header must be 12 bytes");
      const flags = buf.readUInt16BE(offset + 2);
      return {
        id: buf.readUInt16BE(offset),
        type: flags & RESPONSE_FLAG ? "response" : "query",
        flags: flags & 32767,
        flag_qr: (flags >> 15 & 1) === 1,
        opcode: opcodes.toString(flags >> 11 & 15),
        flag_aa: (flags >> 10 & 1) === 1,
        flag_tc: (flags >> 9 & 1) === 1,
        flag_rd: (flags >> 8 & 1) === 1,
        flag_ra: (flags >> 7 & 1) === 1,
        flag_z: (flags >> 6 & 1) === 1,
        flag_ad: (flags >> 5 & 1) === 1,
        flag_cd: (flags >> 4 & 1) === 1,
        rcode: rcodes.toString(flags & 15),
        questions: new Array(buf.readUInt16BE(offset + 4)),
        answers: new Array(buf.readUInt16BE(offset + 6)),
        authorities: new Array(buf.readUInt16BE(offset + 8)),
        additionals: new Array(buf.readUInt16BE(offset + 10))
      };
    };
    header.decode.bytes = 12;
    header.encodingLength = function() {
      return 12;
    };
    var runknown = exports.unknown = {};
    runknown.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(runknown.encodingLength(data));
      if (!offset) offset = 0;
      buf.writeUInt16BE(data.length, offset);
      data.copy(buf, offset + 2);
      runknown.encode.bytes = data.length + 2;
      return buf;
    };
    runknown.encode.bytes = 0;
    runknown.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const len = buf.readUInt16BE(offset);
      const data = buf.slice(offset + 2, offset + 2 + len);
      runknown.decode.bytes = len + 2;
      return data;
    };
    runknown.decode.bytes = 0;
    runknown.encodingLength = function(data) {
      return data.length + 2;
    };
    var rns = exports.ns = {};
    rns.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rns.encodingLength(data));
      if (!offset) offset = 0;
      name.encode(data, buf, offset + 2);
      buf.writeUInt16BE(name.encode.bytes, offset);
      rns.encode.bytes = name.encode.bytes + 2;
      return buf;
    };
    rns.encode.bytes = 0;
    rns.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const len = buf.readUInt16BE(offset);
      const dd = name.decode(buf, offset + 2);
      rns.decode.bytes = len + 2;
      return dd;
    };
    rns.decode.bytes = 0;
    rns.encodingLength = function(data) {
      return name.encodingLength(data) + 2;
    };
    var rsoa = exports.soa = {};
    rsoa.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rsoa.encodingLength(data));
      if (!offset) offset = 0;
      const oldOffset = offset;
      offset += 2;
      name.encode(data.mname, buf, offset);
      offset += name.encode.bytes;
      name.encode(data.rname, buf, offset, { mail: true });
      offset += name.encode.bytes;
      buf.writeUInt32BE(data.serial || 0, offset);
      offset += 4;
      buf.writeUInt32BE(data.refresh || 0, offset);
      offset += 4;
      buf.writeUInt32BE(data.retry || 0, offset);
      offset += 4;
      buf.writeUInt32BE(data.expire || 0, offset);
      offset += 4;
      buf.writeUInt32BE(data.minimum || 0, offset);
      offset += 4;
      buf.writeUInt16BE(offset - oldOffset - 2, oldOffset);
      rsoa.encode.bytes = offset - oldOffset;
      return buf;
    };
    rsoa.encode.bytes = 0;
    rsoa.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const data = {};
      offset += 2;
      data.mname = name.decode(buf, offset);
      offset += name.decode.bytes;
      data.rname = name.decode(buf, offset, { mail: true });
      offset += name.decode.bytes;
      data.serial = buf.readUInt32BE(offset);
      offset += 4;
      data.refresh = buf.readUInt32BE(offset);
      offset += 4;
      data.retry = buf.readUInt32BE(offset);
      offset += 4;
      data.expire = buf.readUInt32BE(offset);
      offset += 4;
      data.minimum = buf.readUInt32BE(offset);
      offset += 4;
      rsoa.decode.bytes = offset - oldOffset;
      return data;
    };
    rsoa.decode.bytes = 0;
    rsoa.encodingLength = function(data) {
      return 22 + name.encodingLength(data.mname) + name.encodingLength(data.rname);
    };
    var rtxt = exports.txt = {};
    rtxt.encode = function(data, buf, offset) {
      if (!Array.isArray(data)) data = [data];
      for (let i = 0; i < data.length; i++) {
        if (typeof data[i] === "string") {
          data[i] = Buffer2.from(data[i]);
        }
        if (!Buffer2.isBuffer(data[i])) {
          throw new Error("Must be a Buffer");
        }
      }
      if (!buf) buf = Buffer2.alloc(rtxt.encodingLength(data));
      if (!offset) offset = 0;
      const oldOffset = offset;
      offset += 2;
      data.forEach(function(d) {
        buf[offset++] = d.length;
        d.copy(buf, offset, 0, d.length);
        offset += d.length;
      });
      buf.writeUInt16BE(offset - oldOffset - 2, oldOffset);
      rtxt.encode.bytes = offset - oldOffset;
      return buf;
    };
    rtxt.encode.bytes = 0;
    rtxt.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      let remaining = buf.readUInt16BE(offset);
      offset += 2;
      let data = [];
      while (remaining > 0) {
        const len = buf[offset++];
        --remaining;
        if (remaining < len) {
          throw new Error("Buffer overflow");
        }
        data.push(buf.slice(offset, offset + len));
        offset += len;
        remaining -= len;
      }
      rtxt.decode.bytes = offset - oldOffset;
      return data;
    };
    rtxt.decode.bytes = 0;
    rtxt.encodingLength = function(data) {
      if (!Array.isArray(data)) data = [data];
      let length = 2;
      data.forEach(function(buf) {
        if (typeof buf === "string") {
          length += Buffer2.byteLength(buf) + 1;
        } else {
          length += buf.length + 1;
        }
      });
      return length;
    };
    var rnull = exports.null = {};
    rnull.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rnull.encodingLength(data));
      if (!offset) offset = 0;
      if (typeof data === "string") data = Buffer2.from(data);
      if (!data) data = Buffer2.alloc(0);
      const oldOffset = offset;
      offset += 2;
      const len = data.length;
      data.copy(buf, offset, 0, len);
      offset += len;
      buf.writeUInt16BE(offset - oldOffset - 2, oldOffset);
      rnull.encode.bytes = offset - oldOffset;
      return buf;
    };
    rnull.encode.bytes = 0;
    rnull.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const len = buf.readUInt16BE(offset);
      offset += 2;
      const data = buf.slice(offset, offset + len);
      offset += len;
      rnull.decode.bytes = offset - oldOffset;
      return data;
    };
    rnull.decode.bytes = 0;
    rnull.encodingLength = function(data) {
      if (!data) return 2;
      return (Buffer2.isBuffer(data) ? data.length : Buffer2.byteLength(data)) + 2;
    };
    var rhinfo = exports.hinfo = {};
    rhinfo.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rhinfo.encodingLength(data));
      if (!offset) offset = 0;
      const oldOffset = offset;
      offset += 2;
      string2.encode(data.cpu, buf, offset);
      offset += string2.encode.bytes;
      string2.encode(data.os, buf, offset);
      offset += string2.encode.bytes;
      buf.writeUInt16BE(offset - oldOffset - 2, oldOffset);
      rhinfo.encode.bytes = offset - oldOffset;
      return buf;
    };
    rhinfo.encode.bytes = 0;
    rhinfo.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const data = {};
      offset += 2;
      data.cpu = string2.decode(buf, offset);
      offset += string2.decode.bytes;
      data.os = string2.decode(buf, offset);
      offset += string2.decode.bytes;
      rhinfo.decode.bytes = offset - oldOffset;
      return data;
    };
    rhinfo.decode.bytes = 0;
    rhinfo.encodingLength = function(data) {
      return string2.encodingLength(data.cpu) + string2.encodingLength(data.os) + 2;
    };
    var rptr = exports.ptr = {};
    var rcname = exports.cname = rptr;
    var rdname = exports.dname = rptr;
    rptr.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rptr.encodingLength(data));
      if (!offset) offset = 0;
      name.encode(data, buf, offset + 2);
      buf.writeUInt16BE(name.encode.bytes, offset);
      rptr.encode.bytes = name.encode.bytes + 2;
      return buf;
    };
    rptr.encode.bytes = 0;
    rptr.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const data = name.decode(buf, offset + 2);
      rptr.decode.bytes = name.decode.bytes + 2;
      return data;
    };
    rptr.decode.bytes = 0;
    rptr.encodingLength = function(data) {
      return name.encodingLength(data) + 2;
    };
    var rsrv = exports.srv = {};
    rsrv.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rsrv.encodingLength(data));
      if (!offset) offset = 0;
      buf.writeUInt16BE(data.priority || 0, offset + 2);
      buf.writeUInt16BE(data.weight || 0, offset + 4);
      buf.writeUInt16BE(data.port || 0, offset + 6);
      name.encode(data.target, buf, offset + 8);
      const len = name.encode.bytes + 6;
      buf.writeUInt16BE(len, offset);
      rsrv.encode.bytes = len + 2;
      return buf;
    };
    rsrv.encode.bytes = 0;
    rsrv.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const len = buf.readUInt16BE(offset);
      const data = {};
      data.priority = buf.readUInt16BE(offset + 2);
      data.weight = buf.readUInt16BE(offset + 4);
      data.port = buf.readUInt16BE(offset + 6);
      data.target = name.decode(buf, offset + 8);
      rsrv.decode.bytes = len + 2;
      return data;
    };
    rsrv.decode.bytes = 0;
    rsrv.encodingLength = function(data) {
      return 8 + name.encodingLength(data.target);
    };
    var rcaa = exports.caa = {};
    rcaa.ISSUER_CRITICAL = 1 << 7;
    rcaa.encode = function(data, buf, offset) {
      const len = rcaa.encodingLength(data);
      if (!buf) buf = Buffer2.alloc(rcaa.encodingLength(data));
      if (!offset) offset = 0;
      if (data.issuerCritical) {
        data.flags = rcaa.ISSUER_CRITICAL;
      }
      buf.writeUInt16BE(len - 2, offset);
      offset += 2;
      buf.writeUInt8(data.flags || 0, offset);
      offset += 1;
      string2.encode(data.tag, buf, offset);
      offset += string2.encode.bytes;
      buf.write(data.value, offset);
      offset += Buffer2.byteLength(data.value);
      rcaa.encode.bytes = len;
      return buf;
    };
    rcaa.encode.bytes = 0;
    rcaa.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const len = buf.readUInt16BE(offset);
      offset += 2;
      const oldOffset = offset;
      const data = {};
      data.flags = buf.readUInt8(offset);
      offset += 1;
      data.tag = string2.decode(buf, offset);
      offset += string2.decode.bytes;
      data.value = buf.toString("utf-8", offset, oldOffset + len);
      data.issuerCritical = !!(data.flags & rcaa.ISSUER_CRITICAL);
      rcaa.decode.bytes = len + 2;
      return data;
    };
    rcaa.decode.bytes = 0;
    rcaa.encodingLength = function(data) {
      return string2.encodingLength(data.tag) + string2.encodingLength(data.value) + 2;
    };
    var rmx = exports.mx = {};
    rmx.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rmx.encodingLength(data));
      if (!offset) offset = 0;
      const oldOffset = offset;
      offset += 2;
      buf.writeUInt16BE(data.preference || 0, offset);
      offset += 2;
      name.encode(data.exchange, buf, offset);
      offset += name.encode.bytes;
      buf.writeUInt16BE(offset - oldOffset - 2, oldOffset);
      rmx.encode.bytes = offset - oldOffset;
      return buf;
    };
    rmx.encode.bytes = 0;
    rmx.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const data = {};
      offset += 2;
      data.preference = buf.readUInt16BE(offset);
      offset += 2;
      data.exchange = name.decode(buf, offset);
      offset += name.decode.bytes;
      rmx.decode.bytes = offset - oldOffset;
      return data;
    };
    rmx.encodingLength = function(data) {
      return 4 + name.encodingLength(data.exchange);
    };
    var ra = exports.a = {};
    ra.encode = function(host, buf, offset) {
      if (!buf) buf = Buffer2.alloc(ra.encodingLength(host));
      if (!offset) offset = 0;
      buf.writeUInt16BE(4, offset);
      offset += 2;
      ip.v4.encode(host, buf, offset);
      ra.encode.bytes = 6;
      return buf;
    };
    ra.encode.bytes = 0;
    ra.decode = function(buf, offset) {
      if (!offset) offset = 0;
      offset += 2;
      const host = ip.v4.decode(buf, offset);
      ra.decode.bytes = 6;
      return host;
    };
    ra.decode.bytes = 0;
    ra.encodingLength = function() {
      return 6;
    };
    var raaaa = exports.aaaa = {};
    raaaa.encode = function(host, buf, offset) {
      if (!buf) buf = Buffer2.alloc(raaaa.encodingLength(host));
      if (!offset) offset = 0;
      buf.writeUInt16BE(16, offset);
      offset += 2;
      ip.v6.encode(host, buf, offset);
      raaaa.encode.bytes = 18;
      return buf;
    };
    raaaa.encode.bytes = 0;
    raaaa.decode = function(buf, offset) {
      if (!offset) offset = 0;
      offset += 2;
      const host = ip.v6.decode(buf, offset);
      raaaa.decode.bytes = 18;
      return host;
    };
    raaaa.decode.bytes = 0;
    raaaa.encodingLength = function() {
      return 18;
    };
    var roption = exports.option = {};
    roption.encode = function(option3, buf, offset) {
      if (!buf) buf = Buffer2.alloc(roption.encodingLength(option3));
      if (!offset) offset = 0;
      const oldOffset = offset;
      const code = optioncodes.toCode(option3.code);
      buf.writeUInt16BE(code, offset);
      offset += 2;
      if (option3.data) {
        buf.writeUInt16BE(option3.data.length, offset);
        offset += 2;
        option3.data.copy(buf, offset);
        offset += option3.data.length;
      } else {
        switch (code) {
          // case 3: NSID.  No encode makes sense.
          // case 5,6,7: Not implementable
          case 8:
            const spl = option3.sourcePrefixLength || 0;
            const fam = option3.family || ip.familyOf(option3.ip);
            const ipBuf = ip.encode(option3.ip, Buffer2.alloc);
            const ipLen = Math.ceil(spl / 8);
            buf.writeUInt16BE(ipLen + 4, offset);
            offset += 2;
            buf.writeUInt16BE(fam, offset);
            offset += 2;
            buf.writeUInt8(spl, offset++);
            buf.writeUInt8(option3.scopePrefixLength || 0, offset++);
            ipBuf.copy(buf, offset, 0, ipLen);
            offset += ipLen;
            break;
          // case 9: EXPIRE (experimental)
          // case 10: COOKIE.  No encode makes sense.
          case 11:
            if (option3.timeout) {
              buf.writeUInt16BE(2, offset);
              offset += 2;
              buf.writeUInt16BE(option3.timeout, offset);
              offset += 2;
            } else {
              buf.writeUInt16BE(0, offset);
              offset += 2;
            }
            break;
          case 12:
            const len = option3.length || 0;
            buf.writeUInt16BE(len, offset);
            offset += 2;
            buf.fill(0, offset, offset + len);
            offset += len;
            break;
          // case 13:  CHAIN.  Experimental.
          case 14:
            const tagsLen = option3.tags.length * 2;
            buf.writeUInt16BE(tagsLen, offset);
            offset += 2;
            for (const tag of option3.tags) {
              buf.writeUInt16BE(tag, offset);
              offset += 2;
            }
            break;
          default:
            throw new Error(`Unknown roption code: ${option3.code}`);
        }
      }
      roption.encode.bytes = offset - oldOffset;
      return buf;
    };
    roption.encode.bytes = 0;
    roption.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const option3 = {};
      option3.code = buf.readUInt16BE(offset);
      option3.type = optioncodes.toString(option3.code);
      offset += 2;
      const len = buf.readUInt16BE(offset);
      offset += 2;
      option3.data = buf.slice(offset, offset + len);
      switch (option3.code) {
        // case 3: NSID.  No decode makes sense.
        case 8:
          option3.family = buf.readUInt16BE(offset);
          offset += 2;
          option3.sourcePrefixLength = buf.readUInt8(offset++);
          option3.scopePrefixLength = buf.readUInt8(offset++);
          const padded = Buffer2.alloc(option3.family === 1 ? 4 : 16);
          buf.copy(padded, 0, offset, offset + len - 4);
          option3.ip = ip.decode(padded);
          break;
        // case 12: Padding.  No decode makes sense.
        case 11:
          if (len > 0) {
            option3.timeout = buf.readUInt16BE(offset);
            offset += 2;
          }
          break;
        case 14:
          option3.tags = [];
          for (let i = 0; i < len; i += 2) {
            option3.tags.push(buf.readUInt16BE(offset));
            offset += 2;
          }
      }
      roption.decode.bytes = len + 4;
      return option3;
    };
    roption.decode.bytes = 0;
    roption.encodingLength = function(option3) {
      if (option3.data) {
        return option3.data.length + 4;
      }
      const code = optioncodes.toCode(option3.code);
      switch (code) {
        case 8:
          const spl = option3.sourcePrefixLength || 0;
          return Math.ceil(spl / 8) + 8;
        case 11:
          return typeof option3.timeout === "number" ? 6 : 4;
        case 12:
          return option3.length + 4;
        case 14:
          return 4 + option3.tags.length * 2;
      }
      throw new Error(`Unknown roption code: ${option3.code}`);
    };
    var ropt = exports.opt = {};
    ropt.encode = function(options, buf, offset) {
      if (!buf) buf = Buffer2.alloc(ropt.encodingLength(options));
      if (!offset) offset = 0;
      const oldOffset = offset;
      const rdlen = encodingLengthList(options, roption);
      buf.writeUInt16BE(rdlen, offset);
      offset = encodeList(options, roption, buf, offset + 2);
      ropt.encode.bytes = offset - oldOffset;
      return buf;
    };
    ropt.encode.bytes = 0;
    ropt.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const options = [];
      let rdlen = buf.readUInt16BE(offset);
      offset += 2;
      let o = 0;
      while (rdlen > 0) {
        options[o++] = roption.decode(buf, offset);
        offset += roption.decode.bytes;
        rdlen -= roption.decode.bytes;
      }
      ropt.decode.bytes = offset - oldOffset;
      return options;
    };
    ropt.decode.bytes = 0;
    ropt.encodingLength = function(options) {
      return 2 + encodingLengthList(options || [], roption);
    };
    var rdnskey = exports.dnskey = {};
    rdnskey.PROTOCOL_DNSSEC = 3;
    rdnskey.ZONE_KEY = 128;
    rdnskey.SECURE_ENTRYPOINT = 32768;
    rdnskey.encode = function(key, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rdnskey.encodingLength(key));
      if (!offset) offset = 0;
      const oldOffset = offset;
      const keydata = key.key;
      if (!Buffer2.isBuffer(keydata)) {
        throw new Error("Key must be a Buffer");
      }
      offset += 2;
      buf.writeUInt16BE(key.flags, offset);
      offset += 2;
      buf.writeUInt8(rdnskey.PROTOCOL_DNSSEC, offset);
      offset += 1;
      buf.writeUInt8(key.algorithm, offset);
      offset += 1;
      keydata.copy(buf, offset, 0, keydata.length);
      offset += keydata.length;
      rdnskey.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rdnskey.encode.bytes - 2, oldOffset);
      return buf;
    };
    rdnskey.encode.bytes = 0;
    rdnskey.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      var key = {};
      var length = buf.readUInt16BE(offset);
      offset += 2;
      key.flags = buf.readUInt16BE(offset);
      offset += 2;
      if (buf.readUInt8(offset) !== rdnskey.PROTOCOL_DNSSEC) {
        throw new Error("Protocol must be 3");
      }
      offset += 1;
      key.algorithm = buf.readUInt8(offset);
      offset += 1;
      key.key = buf.slice(offset, oldOffset + length + 2);
      offset += key.key.length;
      rdnskey.decode.bytes = offset - oldOffset;
      return key;
    };
    rdnskey.decode.bytes = 0;
    rdnskey.encodingLength = function(key) {
      return 6 + Buffer2.byteLength(key.key);
    };
    var rrrsig = exports.rrsig = {};
    rrrsig.encode = function(sig, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rrrsig.encodingLength(sig));
      if (!offset) offset = 0;
      const oldOffset = offset;
      const signature = sig.signature;
      if (!Buffer2.isBuffer(signature)) {
        throw new Error("Signature must be a Buffer");
      }
      offset += 2;
      buf.writeUInt16BE(types.toType(sig.typeCovered), offset);
      offset += 2;
      buf.writeUInt8(sig.algorithm, offset);
      offset += 1;
      buf.writeUInt8(sig.labels, offset);
      offset += 1;
      buf.writeUInt32BE(sig.originalTTL, offset);
      offset += 4;
      buf.writeUInt32BE(sig.expiration, offset);
      offset += 4;
      buf.writeUInt32BE(sig.inception, offset);
      offset += 4;
      buf.writeUInt16BE(sig.keyTag, offset);
      offset += 2;
      name.encode(sig.signersName, buf, offset);
      offset += name.encode.bytes;
      signature.copy(buf, offset, 0, signature.length);
      offset += signature.length;
      rrrsig.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rrrsig.encode.bytes - 2, oldOffset);
      return buf;
    };
    rrrsig.encode.bytes = 0;
    rrrsig.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      var sig = {};
      var length = buf.readUInt16BE(offset);
      offset += 2;
      sig.typeCovered = types.toString(buf.readUInt16BE(offset));
      offset += 2;
      sig.algorithm = buf.readUInt8(offset);
      offset += 1;
      sig.labels = buf.readUInt8(offset);
      offset += 1;
      sig.originalTTL = buf.readUInt32BE(offset);
      offset += 4;
      sig.expiration = buf.readUInt32BE(offset);
      offset += 4;
      sig.inception = buf.readUInt32BE(offset);
      offset += 4;
      sig.keyTag = buf.readUInt16BE(offset);
      offset += 2;
      sig.signersName = name.decode(buf, offset);
      offset += name.decode.bytes;
      sig.signature = buf.slice(offset, oldOffset + length + 2);
      offset += sig.signature.length;
      rrrsig.decode.bytes = offset - oldOffset;
      return sig;
    };
    rrrsig.decode.bytes = 0;
    rrrsig.encodingLength = function(sig) {
      return 20 + name.encodingLength(sig.signersName) + Buffer2.byteLength(sig.signature);
    };
    var rrp = exports.rp = {};
    rrp.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rrp.encodingLength(data));
      if (!offset) offset = 0;
      const oldOffset = offset;
      offset += 2;
      name.encode(data.mbox || ".", buf, offset, { mail: true });
      offset += name.encode.bytes;
      name.encode(data.txt || ".", buf, offset);
      offset += name.encode.bytes;
      rrp.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rrp.encode.bytes - 2, oldOffset);
      return buf;
    };
    rrp.encode.bytes = 0;
    rrp.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const data = {};
      offset += 2;
      data.mbox = name.decode(buf, offset, { mail: true }) || ".";
      offset += name.decode.bytes;
      data.txt = name.decode(buf, offset) || ".";
      offset += name.decode.bytes;
      rrp.decode.bytes = offset - oldOffset;
      return data;
    };
    rrp.decode.bytes = 0;
    rrp.encodingLength = function(data) {
      return 2 + name.encodingLength(data.mbox || ".") + name.encodingLength(data.txt || ".");
    };
    var typebitmap = {};
    typebitmap.encode = function(typelist, buf, offset) {
      if (!buf) buf = Buffer2.alloc(typebitmap.encodingLength(typelist));
      if (!offset) offset = 0;
      const oldOffset = offset;
      var typesByWindow = [];
      for (var i = 0; i < typelist.length; i++) {
        var typeid = types.toType(typelist[i]);
        if (typesByWindow[typeid >> 8] === void 0) {
          typesByWindow[typeid >> 8] = [];
        }
        typesByWindow[typeid >> 8][typeid >> 3 & 31] |= 1 << 7 - (typeid & 7);
      }
      for (i = 0; i < typesByWindow.length; i++) {
        if (typesByWindow[i] !== void 0) {
          var windowBuf = Buffer2.from(typesByWindow[i]);
          buf.writeUInt8(i, offset);
          offset += 1;
          buf.writeUInt8(windowBuf.length, offset);
          offset += 1;
          windowBuf.copy(buf, offset);
          offset += windowBuf.length;
        }
      }
      typebitmap.encode.bytes = offset - oldOffset;
      return buf;
    };
    typebitmap.encode.bytes = 0;
    typebitmap.decode = function(buf, offset, length) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      var typelist = [];
      while (offset - oldOffset < length) {
        var window = buf.readUInt8(offset);
        offset += 1;
        var windowLength = buf.readUInt8(offset);
        offset += 1;
        for (var i = 0; i < windowLength; i++) {
          var b = buf.readUInt8(offset + i);
          for (var j = 0; j < 8; j++) {
            if (b & 1 << 7 - j) {
              var typeid = types.toString(window << 8 | i << 3 | j);
              typelist.push(typeid);
            }
          }
        }
        offset += windowLength;
      }
      typebitmap.decode.bytes = offset - oldOffset;
      return typelist;
    };
    typebitmap.decode.bytes = 0;
    typebitmap.encodingLength = function(typelist) {
      var extents = [];
      for (var i = 0; i < typelist.length; i++) {
        var typeid = types.toType(typelist[i]);
        extents[typeid >> 8] = Math.max(extents[typeid >> 8] || 0, typeid & 255);
      }
      var len = 0;
      for (i = 0; i < extents.length; i++) {
        if (extents[i] !== void 0) {
          len += 2 + Math.ceil((extents[i] + 1) / 8);
        }
      }
      return len;
    };
    var rnsec = exports.nsec = {};
    rnsec.encode = function(record, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rnsec.encodingLength(record));
      if (!offset) offset = 0;
      const oldOffset = offset;
      offset += 2;
      name.encode(record.nextDomain, buf, offset);
      offset += name.encode.bytes;
      typebitmap.encode(record.rrtypes, buf, offset);
      offset += typebitmap.encode.bytes;
      rnsec.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rnsec.encode.bytes - 2, oldOffset);
      return buf;
    };
    rnsec.encode.bytes = 0;
    rnsec.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      var record = {};
      var length = buf.readUInt16BE(offset);
      offset += 2;
      record.nextDomain = name.decode(buf, offset);
      offset += name.decode.bytes;
      record.rrtypes = typebitmap.decode(buf, offset, length - (offset - oldOffset));
      offset += typebitmap.decode.bytes;
      rnsec.decode.bytes = offset - oldOffset;
      return record;
    };
    rnsec.decode.bytes = 0;
    rnsec.encodingLength = function(record) {
      return 2 + name.encodingLength(record.nextDomain) + typebitmap.encodingLength(record.rrtypes);
    };
    var rnsec3 = exports.nsec3 = {};
    rnsec3.encode = function(record, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rnsec3.encodingLength(record));
      if (!offset) offset = 0;
      const oldOffset = offset;
      const salt = record.salt;
      if (!Buffer2.isBuffer(salt)) {
        throw new Error("salt must be a Buffer");
      }
      const nextDomain = record.nextDomain;
      if (!Buffer2.isBuffer(nextDomain)) {
        throw new Error("nextDomain must be a Buffer");
      }
      offset += 2;
      buf.writeUInt8(record.algorithm, offset);
      offset += 1;
      buf.writeUInt8(record.flags, offset);
      offset += 1;
      buf.writeUInt16BE(record.iterations, offset);
      offset += 2;
      buf.writeUInt8(salt.length, offset);
      offset += 1;
      salt.copy(buf, offset, 0, salt.length);
      offset += salt.length;
      buf.writeUInt8(nextDomain.length, offset);
      offset += 1;
      nextDomain.copy(buf, offset, 0, nextDomain.length);
      offset += nextDomain.length;
      typebitmap.encode(record.rrtypes, buf, offset);
      offset += typebitmap.encode.bytes;
      rnsec3.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rnsec3.encode.bytes - 2, oldOffset);
      return buf;
    };
    rnsec3.encode.bytes = 0;
    rnsec3.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      var record = {};
      var length = buf.readUInt16BE(offset);
      offset += 2;
      record.algorithm = buf.readUInt8(offset);
      offset += 1;
      record.flags = buf.readUInt8(offset);
      offset += 1;
      record.iterations = buf.readUInt16BE(offset);
      offset += 2;
      const saltLength = buf.readUInt8(offset);
      offset += 1;
      record.salt = buf.slice(offset, offset + saltLength);
      offset += saltLength;
      const hashLength = buf.readUInt8(offset);
      offset += 1;
      record.nextDomain = buf.slice(offset, offset + hashLength);
      offset += hashLength;
      record.rrtypes = typebitmap.decode(buf, offset, length - (offset - oldOffset));
      offset += typebitmap.decode.bytes;
      rnsec3.decode.bytes = offset - oldOffset;
      return record;
    };
    rnsec3.decode.bytes = 0;
    rnsec3.encodingLength = function(record) {
      return 8 + record.salt.length + record.nextDomain.length + typebitmap.encodingLength(record.rrtypes);
    };
    var rds = exports.ds = {};
    rds.encode = function(digest, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rds.encodingLength(digest));
      if (!offset) offset = 0;
      const oldOffset = offset;
      const digestdata = digest.digest;
      if (!Buffer2.isBuffer(digestdata)) {
        throw new Error("Digest must be a Buffer");
      }
      offset += 2;
      buf.writeUInt16BE(digest.keyTag, offset);
      offset += 2;
      buf.writeUInt8(digest.algorithm, offset);
      offset += 1;
      buf.writeUInt8(digest.digestType, offset);
      offset += 1;
      digestdata.copy(buf, offset, 0, digestdata.length);
      offset += digestdata.length;
      rds.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rds.encode.bytes - 2, oldOffset);
      return buf;
    };
    rds.encode.bytes = 0;
    rds.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      var digest = {};
      var length = buf.readUInt16BE(offset);
      offset += 2;
      digest.keyTag = buf.readUInt16BE(offset);
      offset += 2;
      digest.algorithm = buf.readUInt8(offset);
      offset += 1;
      digest.digestType = buf.readUInt8(offset);
      offset += 1;
      digest.digest = buf.slice(offset, oldOffset + length + 2);
      offset += digest.digest.length;
      rds.decode.bytes = offset - oldOffset;
      return digest;
    };
    rds.decode.bytes = 0;
    rds.encodingLength = function(digest) {
      return 6 + Buffer2.byteLength(digest.digest);
    };
    var rsshfp = exports.sshfp = {};
    rsshfp.getFingerprintLengthForHashType = function getFingerprintLengthForHashType(hashType) {
      switch (hashType) {
        case 1:
          return 20;
        case 2:
          return 32;
      }
    };
    rsshfp.encode = function encode(record, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rsshfp.encodingLength(record));
      if (!offset) offset = 0;
      const oldOffset = offset;
      offset += 2;
      buf[offset] = record.algorithm;
      offset += 1;
      buf[offset] = record.hash;
      offset += 1;
      const fingerprintBuf = Buffer2.from(record.fingerprint.toUpperCase(), "hex");
      if (fingerprintBuf.length !== rsshfp.getFingerprintLengthForHashType(record.hash)) {
        throw new Error("Invalid fingerprint length");
      }
      fingerprintBuf.copy(buf, offset);
      offset += fingerprintBuf.byteLength;
      rsshfp.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rsshfp.encode.bytes - 2, oldOffset);
      return buf;
    };
    rsshfp.encode.bytes = 0;
    rsshfp.decode = function decode(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const record = {};
      offset += 2;
      record.algorithm = buf[offset];
      offset += 1;
      record.hash = buf[offset];
      offset += 1;
      const fingerprintLength = rsshfp.getFingerprintLengthForHashType(record.hash);
      record.fingerprint = buf.slice(offset, offset + fingerprintLength).toString("hex").toUpperCase();
      offset += fingerprintLength;
      rsshfp.decode.bytes = offset - oldOffset;
      return record;
    };
    rsshfp.decode.bytes = 0;
    rsshfp.encodingLength = function(record) {
      return 4 + Buffer2.from(record.fingerprint, "hex").byteLength;
    };
    var rnaptr = exports.naptr = {};
    rnaptr.encode = function(data, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rnaptr.encodingLength(data));
      if (!offset) offset = 0;
      const oldOffset = offset;
      offset += 2;
      buf.writeUInt16BE(data.order || 0, offset);
      offset += 2;
      buf.writeUInt16BE(data.preference || 0, offset);
      offset += 2;
      string2.encode(data.flags, buf, offset);
      offset += string2.encode.bytes;
      string2.encode(data.services, buf, offset);
      offset += string2.encode.bytes;
      string2.encode(data.regexp, buf, offset);
      offset += string2.encode.bytes;
      name.encode(data.replacement, buf, offset);
      offset += name.encode.bytes;
      rnaptr.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rnaptr.encode.bytes - 2, oldOffset);
      return buf;
    };
    rnaptr.encode.bytes = 0;
    rnaptr.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const data = {};
      offset += 2;
      data.order = buf.readUInt16BE(offset);
      offset += 2;
      data.preference = buf.readUInt16BE(offset);
      offset += 2;
      data.flags = string2.decode(buf, offset);
      offset += string2.decode.bytes;
      data.services = string2.decode(buf, offset);
      offset += string2.decode.bytes;
      data.regexp = string2.decode(buf, offset);
      offset += string2.decode.bytes;
      data.replacement = name.decode(buf, offset);
      offset += name.decode.bytes;
      rnaptr.decode.bytes = offset - oldOffset;
      return data;
    };
    rnaptr.decode.bytes = 0;
    rnaptr.encodingLength = function(data) {
      return string2.encodingLength(data.flags) + string2.encodingLength(data.services) + string2.encodingLength(data.regexp) + name.encodingLength(data.replacement) + 6;
    };
    var rtlsa = exports.tlsa = {};
    rtlsa.encode = function(cert, buf, offset) {
      if (!buf) buf = Buffer2.alloc(rtlsa.encodingLength(cert));
      if (!offset) offset = 0;
      const oldOffset = offset;
      const certdata = cert.certificate;
      if (!Buffer2.isBuffer(certdata)) {
        throw new Error("Certificate must be a Buffer");
      }
      offset += 2;
      buf.writeUInt8(cert.usage, offset);
      offset += 1;
      buf.writeUInt8(cert.selector, offset);
      offset += 1;
      buf.writeUInt8(cert.matchingType, offset);
      offset += 1;
      certdata.copy(buf, offset, 0, certdata.length);
      offset += certdata.length;
      rtlsa.encode.bytes = offset - oldOffset;
      buf.writeUInt16BE(rtlsa.encode.bytes - 2, oldOffset);
      return buf;
    };
    rtlsa.encode.bytes = 0;
    rtlsa.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const cert = {};
      const length = buf.readUInt16BE(offset);
      offset += 2;
      cert.usage = buf.readUInt8(offset);
      offset += 1;
      cert.selector = buf.readUInt8(offset);
      offset += 1;
      cert.matchingType = buf.readUInt8(offset);
      offset += 1;
      cert.certificate = buf.slice(offset, oldOffset + length + 2);
      offset += cert.certificate.length;
      rtlsa.decode.bytes = offset - oldOffset;
      return cert;
    };
    rtlsa.decode.bytes = 0;
    rtlsa.encodingLength = function(cert) {
      return 5 + Buffer2.byteLength(cert.certificate);
    };
    var renc = exports.record = function(type) {
      switch (type.toUpperCase()) {
        case "A":
          return ra;
        case "PTR":
          return rptr;
        case "CNAME":
          return rcname;
        case "DNAME":
          return rdname;
        case "TXT":
          return rtxt;
        case "NULL":
          return rnull;
        case "AAAA":
          return raaaa;
        case "SRV":
          return rsrv;
        case "HINFO":
          return rhinfo;
        case "CAA":
          return rcaa;
        case "NS":
          return rns;
        case "SOA":
          return rsoa;
        case "MX":
          return rmx;
        case "OPT":
          return ropt;
        case "DNSKEY":
          return rdnskey;
        case "RRSIG":
          return rrrsig;
        case "RP":
          return rrp;
        case "NSEC":
          return rnsec;
        case "NSEC3":
          return rnsec3;
        case "SSHFP":
          return rsshfp;
        case "DS":
          return rds;
        case "NAPTR":
          return rnaptr;
        case "TLSA":
          return rtlsa;
      }
      return runknown;
    };
    var answer = exports.answer = {};
    answer.encode = function(a, buf, offset) {
      if (!buf) buf = Buffer2.alloc(answer.encodingLength(a));
      if (!offset) offset = 0;
      const oldOffset = offset;
      name.encode(a.name, buf, offset);
      offset += name.encode.bytes;
      buf.writeUInt16BE(types.toType(a.type), offset);
      if (a.type.toUpperCase() === "OPT") {
        if (a.name !== ".") {
          throw new Error("OPT name must be root.");
        }
        buf.writeUInt16BE(a.udpPayloadSize || 4096, offset + 2);
        buf.writeUInt8(a.extendedRcode || 0, offset + 4);
        buf.writeUInt8(a.ednsVersion || 0, offset + 5);
        buf.writeUInt16BE(a.flags || 0, offset + 6);
        offset += 8;
        ropt.encode(a.options || [], buf, offset);
        offset += ropt.encode.bytes;
      } else {
        let klass = classes.toClass(a.class === void 0 ? "IN" : a.class);
        if (a.flush) klass |= FLUSH_MASK;
        buf.writeUInt16BE(klass, offset + 2);
        buf.writeUInt32BE(a.ttl || 0, offset + 4);
        offset += 8;
        const enc = renc(a.type);
        enc.encode(a.data, buf, offset);
        offset += enc.encode.bytes;
      }
      answer.encode.bytes = offset - oldOffset;
      return buf;
    };
    answer.encode.bytes = 0;
    answer.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const a = {};
      const oldOffset = offset;
      a.name = name.decode(buf, offset);
      offset += name.decode.bytes;
      a.type = types.toString(buf.readUInt16BE(offset));
      if (a.type === "OPT") {
        a.udpPayloadSize = buf.readUInt16BE(offset + 2);
        a.extendedRcode = buf.readUInt8(offset + 4);
        a.ednsVersion = buf.readUInt8(offset + 5);
        a.flags = buf.readUInt16BE(offset + 6);
        a.flag_do = (a.flags >> 15 & 1) === 1;
        a.options = ropt.decode(buf, offset + 8);
        offset += 8 + ropt.decode.bytes;
      } else {
        const klass = buf.readUInt16BE(offset + 2);
        a.ttl = buf.readUInt32BE(offset + 4);
        a.class = classes.toString(klass & NOT_FLUSH_MASK);
        a.flush = !!(klass & FLUSH_MASK);
        const enc = renc(a.type);
        a.data = enc.decode(buf, offset + 8);
        offset += 8 + enc.decode.bytes;
      }
      answer.decode.bytes = offset - oldOffset;
      return a;
    };
    answer.decode.bytes = 0;
    answer.encodingLength = function(a) {
      const data = a.data !== null && a.data !== void 0 ? a.data : a.options;
      return name.encodingLength(a.name) + 8 + renc(a.type).encodingLength(data);
    };
    var question = exports.question = {};
    question.encode = function(q, buf, offset) {
      if (!buf) buf = Buffer2.alloc(question.encodingLength(q));
      if (!offset) offset = 0;
      const oldOffset = offset;
      name.encode(q.name, buf, offset);
      offset += name.encode.bytes;
      buf.writeUInt16BE(types.toType(q.type), offset);
      offset += 2;
      buf.writeUInt16BE(classes.toClass(q.class === void 0 ? "IN" : q.class), offset);
      offset += 2;
      question.encode.bytes = offset - oldOffset;
      return q;
    };
    question.encode.bytes = 0;
    question.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const q = {};
      q.name = name.decode(buf, offset);
      offset += name.decode.bytes;
      q.type = types.toString(buf.readUInt16BE(offset));
      offset += 2;
      q.class = classes.toString(buf.readUInt16BE(offset));
      offset += 2;
      const qu = !!(q.class & QU_MASK);
      if (qu) q.class &= NOT_QU_MASK;
      question.decode.bytes = offset - oldOffset;
      return q;
    };
    question.decode.bytes = 0;
    question.encodingLength = function(q) {
      return name.encodingLength(q.name) + 4;
    };
    exports.AUTHORITATIVE_ANSWER = 1 << 10;
    exports.TRUNCATED_RESPONSE = 1 << 9;
    exports.RECURSION_DESIRED = 1 << 8;
    exports.RECURSION_AVAILABLE = 1 << 7;
    exports.AUTHENTIC_DATA = 1 << 5;
    exports.CHECKING_DISABLED = 1 << 4;
    exports.DNSSEC_OK = 1 << 15;
    exports.encode = function(result3, buf, offset) {
      const allocing = !buf;
      if (allocing) buf = Buffer2.alloc(exports.encodingLength(result3));
      if (!offset) offset = 0;
      const oldOffset = offset;
      if (!result3.questions) result3.questions = [];
      if (!result3.answers) result3.answers = [];
      if (!result3.authorities) result3.authorities = [];
      if (!result3.additionals) result3.additionals = [];
      header.encode(result3, buf, offset);
      offset += header.encode.bytes;
      offset = encodeList(result3.questions, question, buf, offset);
      offset = encodeList(result3.answers, answer, buf, offset);
      offset = encodeList(result3.authorities, answer, buf, offset);
      offset = encodeList(result3.additionals, answer, buf, offset);
      exports.encode.bytes = offset - oldOffset;
      if (allocing && exports.encode.bytes !== buf.length) {
        return buf.slice(0, exports.encode.bytes);
      }
      return buf;
    };
    exports.encode.bytes = 0;
    exports.decode = function(buf, offset) {
      if (!offset) offset = 0;
      const oldOffset = offset;
      const result3 = header.decode(buf, offset);
      offset += header.decode.bytes;
      offset = decodeList(result3.questions, question, buf, offset);
      offset = decodeList(result3.answers, answer, buf, offset);
      offset = decodeList(result3.authorities, answer, buf, offset);
      offset = decodeList(result3.additionals, answer, buf, offset);
      exports.decode.bytes = offset - oldOffset;
      return result3;
    };
    exports.decode.bytes = 0;
    exports.encodingLength = function(result3) {
      return header.encodingLength(result3) + encodingLengthList(result3.questions || [], question) + encodingLengthList(result3.answers || [], answer) + encodingLengthList(result3.authorities || [], answer) + encodingLengthList(result3.additionals || [], answer);
    };
    exports.streamEncode = function(result3) {
      const buf = exports.encode(result3);
      const sbuf = Buffer2.alloc(2);
      sbuf.writeUInt16BE(buf.byteLength);
      const combine2 = Buffer2.concat([sbuf, buf]);
      exports.streamEncode.bytes = combine2.byteLength;
      return combine2;
    };
    exports.streamEncode.bytes = 0;
    exports.streamDecode = function(sbuf) {
      const len = sbuf.readUInt16BE(0);
      if (sbuf.byteLength < len + 2) {
        return null;
      }
      const result3 = exports.decode(sbuf.slice(2));
      exports.streamDecode.bytes = exports.decode.bytes;
      return result3;
    };
    exports.streamDecode.bytes = 0;
    function encodingLengthList(list, enc) {
      let len = 0;
      for (let i = 0; i < list.length; i++) len += enc.encodingLength(list[i]);
      return len;
    }
    function encodeList(list, enc, buf, offset) {
      for (let i = 0; i < list.length; i++) {
        enc.encode(list[i], buf, offset);
        offset += enc.encode.bytes;
      }
      return offset;
    }
    function decodeList(list, enc, buf, offset) {
      for (let i = 0; i < list.length; i++) {
        list[i] = enc.decode(buf, offset);
        offset += enc.decode.bytes;
      }
      return offset;
    }
  }
});

// node_modules/thunky/index.js
var require_thunky = __commonJS({
  "node_modules/thunky/index.js"(exports, module) {
    "use strict";
    var nextTick = nextTickArgs;
    process.nextTick(upgrade, 42);
    module.exports = thunky;
    function thunky(fn3) {
      var state = run;
      return thunk;
      function thunk(callback3) {
        state(callback3 || noop);
      }
      function run(callback3) {
        var stack = [callback3];
        state = wait;
        fn3(done4);
        function wait(callback4) {
          stack.push(callback4);
        }
        function done4(err) {
          var args3 = arguments;
          state = isError(err) ? run : finished;
          while (stack.length) finished(stack.shift());
          function finished(callback4) {
            nextTick(apply2, callback4, args3);
          }
        }
      }
    }
    function isError(err) {
      return Object.prototype.toString.call(err) === "[object Error]";
    }
    function noop() {
    }
    function apply2(callback3, args3) {
      callback3.apply(null, args3);
    }
    function upgrade(val) {
      if (val === 42) nextTick = process.nextTick;
    }
    function nextTickArgs(fn3, a, b) {
      process.nextTick(function() {
        fn3(a, b);
      });
    }
  }
});

// node_modules/multicast-dns/index.js
var require_multicast_dns = __commonJS({
  "node_modules/multicast-dns/index.js"(exports, module) {
    var packet = require_dns_packet();
    var dgram = __require("dgram");
    var thunky = require_thunky();
    var events = __require("events");
    var os = __require("os");
    var noop = function() {
    };
    module.exports = function(opts) {
      if (!opts) opts = {};
      var that = new events.EventEmitter();
      var port = typeof opts.port === "number" ? opts.port : 5353;
      var type = opts.type || "udp4";
      var ip = opts.ip || opts.host || (type === "udp4" ? "224.0.0.251" : null);
      var me = { address: ip, port };
      var memberships = {};
      var destroyed = false;
      var interval = null;
      if (type === "udp6" && (!ip || !opts.interface)) {
        throw new Error("For IPv6 multicast you must specify `ip` and `interface`");
      }
      var socket = opts.socket || dgram.createSocket({
        type,
        reuseAddr: opts.reuseAddr !== false,
        toString: function() {
          return type;
        }
      });
      socket.on("error", function(err) {
        if (err.code === "EACCES" || err.code === "EADDRINUSE") that.emit("error", err);
        else that.emit("warning", err);
      });
      socket.on("message", function(message, rinfo) {
        try {
          message = packet.decode(message);
        } catch (err) {
          that.emit("warning", err);
          return;
        }
        that.emit("packet", message, rinfo);
        if (message.type === "query") that.emit("query", message, rinfo);
        if (message.type === "response") that.emit("response", message, rinfo);
      });
      socket.on("listening", function() {
        if (!port) port = me.port = socket.address().port;
        if (opts.multicast !== false) {
          that.update();
          interval = setInterval(that.update, 5e3);
          socket.setMulticastTTL(opts.ttl || 255);
          socket.setMulticastLoopback(opts.loopback !== false);
        }
      });
      var bind = thunky(function(cb) {
        if (!port || opts.bind === false) return cb(null);
        socket.once("error", cb);
        socket.bind(port, opts.bind || opts.interface, function() {
          socket.removeListener("error", cb);
          cb(null);
        });
      });
      bind(function(err) {
        if (err) return that.emit("error", err);
        that.emit("ready");
      });
      that.send = function(value, rinfo, cb) {
        if (typeof rinfo === "function") return that.send(value, null, rinfo);
        if (!cb) cb = noop;
        if (!rinfo) rinfo = me;
        else if (!rinfo.host && !rinfo.address) rinfo.address = me.address;
        bind(onbind);
        function onbind(err) {
          if (destroyed) return cb();
          if (err) return cb(err);
          var message = packet.encode(value);
          socket.send(message, 0, message.length, rinfo.port, rinfo.address || rinfo.host, cb);
        }
      };
      that.response = that.respond = function(res, rinfo, cb) {
        if (Array.isArray(res)) res = { answers: res };
        res.type = "response";
        res.flags = (res.flags || 0) | packet.AUTHORITATIVE_ANSWER;
        that.send(res, rinfo, cb);
      };
      that.query = function(q, type2, rinfo, cb) {
        if (typeof type2 === "function") return that.query(q, null, null, type2);
        if (typeof type2 === "object" && type2 && type2.port) return that.query(q, null, type2, rinfo);
        if (typeof rinfo === "function") return that.query(q, type2, null, rinfo);
        if (!cb) cb = noop;
        if (typeof q === "string") q = [{ name: q, type: type2 || "ANY" }];
        if (Array.isArray(q)) q = { type: "query", questions: q };
        q.type = "query";
        that.send(q, rinfo, cb);
      };
      that.destroy = function(cb) {
        if (!cb) cb = noop;
        if (destroyed) return process.nextTick(cb);
        destroyed = true;
        clearInterval(interval);
        for (var iface in memberships) {
          try {
            socket.dropMembership(ip, iface);
          } catch (e) {
          }
        }
        memberships = {};
        socket.close(cb);
      };
      that.update = function() {
        var ifaces = opts.interface ? [].concat(opts.interface) : allInterfaces();
        var updated = false;
        for (var i = 0; i < ifaces.length; i++) {
          var addr = ifaces[i];
          if (memberships[addr]) continue;
          try {
            socket.addMembership(ip, addr);
            memberships[addr] = true;
            updated = true;
          } catch (err) {
            that.emit("warning", err);
          }
        }
        if (updated) {
          if (socket.setMulticastInterface) {
            try {
              socket.setMulticastInterface(opts.interface || defaultInterface());
            } catch (err) {
              that.emit("warning", err);
            }
          }
          that.emit("networkInterface");
        }
      };
      return that;
    };
    function defaultInterface() {
      var networks = os.networkInterfaces();
      var names = Object.keys(networks);
      for (var i = 0; i < names.length; i++) {
        var net = networks[names[i]];
        for (var j = 0; j < net.length; j++) {
          var iface = net[j];
          if (isIPv4(iface.family) && !iface.internal) {
            if (os.platform() === "darwin" && names[i] === "en0") return iface.address;
            return "0.0.0.0";
          }
        }
      }
      return "127.0.0.1";
    }
    function allInterfaces() {
      var networks = os.networkInterfaces();
      var names = Object.keys(networks);
      var res = [];
      for (var i = 0; i < names.length; i++) {
        var net = networks[names[i]];
        for (var j = 0; j < net.length; j++) {
          var iface = net[j];
          if (isIPv4(iface.family)) {
            res.push(iface.address);
            break;
          }
        }
      }
      return res;
    }
    function isIPv4(family) {
      return family === 4 || family === "IPv4";
    }
  }
});

// node_modules/fast-deep-equal/es6/index.js
var require_es6 = __commonJS({
  "node_modules/fast-deep-equal/es6/index.js"(exports, module) {
    "use strict";
    module.exports = function equal(a, b) {
      if (a === b) return true;
      if (a && b && typeof a == "object" && typeof b == "object") {
        if (a.constructor !== b.constructor) return false;
        var length, i, keys;
        if (Array.isArray(a)) {
          length = a.length;
          if (length != b.length) return false;
          for (i = length; i-- !== 0; )
            if (!equal(a[i], b[i])) return false;
          return true;
        }
        if (a instanceof Map && b instanceof Map) {
          if (a.size !== b.size) return false;
          for (i of a.entries())
            if (!b.has(i[0])) return false;
          for (i of a.entries())
            if (!equal(i[1], b.get(i[0]))) return false;
          return true;
        }
        if (a instanceof Set && b instanceof Set) {
          if (a.size !== b.size) return false;
          for (i of a.entries())
            if (!b.has(i[0])) return false;
          return true;
        }
        if (ArrayBuffer.isView(a) && ArrayBuffer.isView(b)) {
          length = a.length;
          if (length != b.length) return false;
          for (i = length; i-- !== 0; )
            if (a[i] !== b[i]) return false;
          return true;
        }
        if (a.constructor === RegExp) return a.source === b.source && a.flags === b.flags;
        if (a.valueOf !== Object.prototype.valueOf) return a.valueOf() === b.valueOf();
        if (a.toString !== Object.prototype.toString) return a.toString() === b.toString();
        keys = Object.keys(a);
        length = keys.length;
        if (length !== Object.keys(b).length) return false;
        for (i = length; i-- !== 0; )
          if (!Object.prototype.hasOwnProperty.call(b, keys[i])) return false;
        for (i = length; i-- !== 0; ) {
          var key = keys[i];
          if (!equal(a[key], b[key])) return false;
        }
        return true;
      }
      return a !== a && b !== b;
    };
  }
});

// node_modules/bonjour-service/dist/lib/mdns-server.js
var require_mdns_server = __commonJS({
  "node_modules/bonjour-service/dist/lib/mdns-server.js"(exports) {
    "use strict";
    var __importDefault = exports && exports.__importDefault || function(mod) {
      return mod && mod.__esModule ? mod : { "default": mod };
    };
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Server = void 0;
    var multicast_dns_1 = __importDefault(require_multicast_dns());
    var es6_1 = __importDefault(require_es6());
    var dns_equal_1 = __importDefault(require_dns_equal());
    var Server = class {
      constructor(opts, errorCallback) {
        this.registry = {};
        this.mdns = (0, multicast_dns_1.default)(opts);
        this.mdns.setMaxListeners(0);
        this.mdns.on("query", this.respondToQuery.bind(this));
        this.errorCallback = errorCallback !== null && errorCallback !== void 0 ? errorCallback : function(err) {
          throw err;
        };
      }
      register(records) {
        const shouldRegister = (record) => {
          var subRegistry = this.registry[record.type];
          if (!subRegistry) {
            subRegistry = this.registry[record.type] = [];
          } else if (subRegistry.some(this.isDuplicateRecord(record))) {
            return;
          }
          subRegistry.push(record);
        };
        if (Array.isArray(records)) {
          records.forEach(shouldRegister);
        } else {
          shouldRegister(records);
        }
      }
      unregister(records) {
        const shouldUnregister = (record) => {
          let type = record.type;
          if (!(type in this.registry)) {
            return;
          }
          this.registry[type] = this.registry[type].filter((i) => i.name !== record.name);
        };
        if (Array.isArray(records)) {
          records.forEach(shouldUnregister);
        } else {
          shouldUnregister(records);
        }
      }
      respondToQuery(query) {
        let self = this;
        query.questions.forEach((question) => {
          var type = question.type;
          var name = question.name;
          var answers = type === "ANY" ? Object.keys(self.registry).map(self.recordsFor.bind(self, name)).flat(1) : self.recordsFor(name, type);
          if (answers.length === 0)
            return;
          var additionals = [];
          if (type !== "ANY") {
            answers.forEach((answer) => {
              if (answer.type !== "PTR")
                return;
              additionals = additionals.concat(self.recordsFor(answer.data, "SRV")).concat(self.recordsFor(answer.data, "TXT"));
            });
            additionals.filter(function(record) {
              return record.type === "SRV";
            }).map(function(record) {
              return record.data.target;
            }).filter(this.unique()).forEach(function(target) {
              additionals = additionals.concat(self.recordsFor(target, "A")).concat(self.recordsFor(target, "AAAA"));
            });
          }
          self.mdns.respond({ answers, additionals }, (err) => {
            if (err) {
              this.errorCallback(err);
            }
          });
        });
      }
      recordsFor(name, type) {
        if (!(type in this.registry)) {
          return [];
        }
        return this.registry[type].filter((record) => {
          var _name = ~name.indexOf(".") ? record.name : record.name.split(".")[0];
          return (0, dns_equal_1.default)(_name, name);
        });
      }
      isDuplicateRecord(a) {
        return (b) => {
          return a.type === b.type && a.name === b.name && (0, es6_1.default)(a.data, b.data);
        };
      }
      unique() {
        var set = [];
        return (obj) => {
          if (~set.indexOf(obj))
            return false;
          set.push(obj);
          return true;
        };
      }
    };
    exports.Server = Server;
    exports.default = Server;
  }
});

// node_modules/bonjour-service/dist/lib/utils/filter-service.js
var require_filter_service = __commonJS({
  "node_modules/bonjour-service/dist/lib/utils/filter-service.js"(exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.default = (service3, txtQuery) => {
      if (txtQuery === void 0)
        return true;
      let serviceTxt = service3.txt;
      let query = Object.entries(txtQuery).map(([key, value]) => {
        let queryValue = serviceTxt[key];
        if (queryValue === void 0)
          return false;
        if (value != queryValue)
          return false;
        return true;
      });
      if (query.length == 0)
        return true;
      if (query.includes(false))
        return false;
      return true;
    };
  }
});

// node_modules/bonjour-service/dist/lib/utils/filter-txt.js
var require_filter_txt = __commonJS({
  "node_modules/bonjour-service/dist/lib/utils/filter-txt.js"(exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.default = (data) => Object.keys(data).filter((key) => !key.includes("binary")).reduce((cur, key) => {
      return Object.assign(cur, { [key]: data[key] });
    }, {});
  }
});

// node_modules/bonjour-service/dist/lib/utils/equal-txt.js
var require_equal_txt = __commonJS({
  "node_modules/bonjour-service/dist/lib/utils/equal-txt.js"(exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.default = equalTxt;
    function equalTxt(a, b) {
      if (a === void 0 || b === void 0)
        return false;
      let aKeys = Object.keys(a);
      let bKeys = Object.keys(b);
      if (aKeys.length != bKeys.length)
        return false;
      for (let key of aKeys) {
        if (a[key] != b[key])
          return false;
      }
      return true;
    }
  }
});

// node_modules/bonjour-service/dist/lib/browser.js
var require_browser = __commonJS({
  "node_modules/bonjour-service/dist/lib/browser.js"(exports) {
    "use strict";
    var __importDefault = exports && exports.__importDefault || function(mod) {
      return mod && mod.__esModule ? mod : { "default": mod };
    };
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Browser = void 0;
    var dns_txt_1 = __importDefault(require_dns_txt());
    var dns_equal_1 = __importDefault(require_dns_equal());
    var events_1 = __require("events");
    var service_types_1 = require_service_types();
    var filter_service_1 = __importDefault(require_filter_service());
    var filter_txt_1 = __importDefault(require_filter_txt());
    var equal_txt_1 = __importDefault(require_equal_txt());
    var TLD = ".local";
    var WILDCARD = "_services._dns-sd._udp" + TLD;
    var Browser2 = class _Browser extends events_1.EventEmitter {
      constructor(mdns, opts, onup) {
        super();
        this.onresponse = void 0;
        this.serviceMap = {};
        this.wildcard = false;
        this._services = [];
        if (typeof opts === "function")
          return new _Browser(mdns, null, opts);
        this.mdns = mdns;
        this.txt = new dns_txt_1.default(opts !== null && opts.txt != null ? opts.txt : void 0);
        if (opts === null || opts.type === void 0) {
          this.name = WILDCARD;
          this.wildcard = true;
        } else {
          this.name = (0, service_types_1.toString)({ name: opts.type, protocol: opts.protocol || "tcp" }) + TLD;
          if (opts.name)
            this.name = opts.name + "." + this.name;
          this.wildcard = false;
        }
        if (opts != null && opts.txt !== void 0)
          this.txtQuery = (0, filter_txt_1.default)(opts.txt);
        if (onup)
          this.on("up", onup);
        this.start();
      }
      start() {
        if (this.onresponse || this.name === void 0)
          return;
        var self = this;
        var nameMap = {};
        if (!this.wildcard)
          nameMap[this.name] = true;
        this.onresponse = (packet, rinfo) => {
          if (self.wildcard) {
            packet.answers.forEach((answer) => {
              if (answer.type !== "PTR" || answer.name !== self.name || answer.name in nameMap)
                return;
              nameMap[answer.data] = true;
              self.mdns.query(answer.data, "PTR");
            });
          }
          Object.keys(nameMap).forEach(function(name) {
            self.goodbyes(name, packet).forEach(self.removeService.bind(self));
            var matches = self.buildServicesFor(name, packet, self.txt, rinfo);
            if (matches.length === 0)
              return;
            matches.forEach((service3) => {
              if (self.serviceMap[service3.fqdn]) {
                self.updateService(service3);
                return;
              }
              self.addService(service3);
            });
          });
        };
        this.mdns.on("response", this.onresponse);
        this.update();
      }
      stop() {
        if (!this.onresponse)
          return;
        this.mdns.removeListener("response", this.onresponse);
        this.onresponse = void 0;
      }
      update() {
        this.mdns.query(this.name, "PTR");
      }
      get services() {
        return this._services;
      }
      addService(service3) {
        if ((0, filter_service_1.default)(service3, this.txtQuery) === false)
          return;
        this._services.push(service3);
        this.serviceMap[service3.fqdn] = true;
        this.emit("up", service3);
      }
      updateService(service3) {
        var _a;
        if ((0, equal_txt_1.default)(service3.txt, ((_a = this._services.find((s) => (0, dns_equal_1.default)(s.fqdn, service3.fqdn))) === null || _a === void 0 ? void 0 : _a.txt) || {}))
          return;
        if (!(0, filter_service_1.default)(service3, this.txtQuery)) {
          this.removeService(service3.fqdn);
          return;
        }
        this._services = this._services.map(function(s) {
          if (!(0, dns_equal_1.default)(s.fqdn, service3.fqdn))
            return s;
          return service3;
        });
        this.emit("txt-update", service3);
      }
      removeService(fqdn) {
        var service3, index;
        this._services.some(function(s, i) {
          if ((0, dns_equal_1.default)(s.fqdn, fqdn)) {
            service3 = s;
            index = i;
            return true;
          }
        });
        if (!service3 || index === void 0)
          return;
        this._services.splice(index, 1);
        delete this.serviceMap[fqdn];
        this.emit("down", service3);
      }
      goodbyes(name, packet) {
        return packet.answers.concat(packet.additionals).filter((rr) => rr.type === "PTR" && rr.ttl === 0 && (0, dns_equal_1.default)(rr.name, name)).map((rr) => rr.data);
      }
      buildServicesFor(name, packet, txt, referer) {
        var records = packet.answers.concat(packet.additionals).filter((rr) => rr.ttl > 0);
        return records.filter((rr) => rr.type === "PTR" && (0, dns_equal_1.default)(rr.name, name)).map((ptr) => {
          const service3 = {
            addresses: [],
            subtypes: []
          };
          records.filter((rr) => {
            return rr.type === "PTR" && (0, dns_equal_1.default)(rr.data, ptr.data) && rr.name.includes("._sub");
          }).forEach((rr) => {
            const types = (0, service_types_1.toType)(rr.name);
            service3.subtypes.push(types.subtype);
          });
          records.filter((rr) => {
            return (rr.type === "SRV" || rr.type === "TXT") && (0, dns_equal_1.default)(rr.name, ptr.data);
          }).forEach((rr) => {
            if (rr.type === "SRV") {
              var parts = rr.name.split(".");
              var name2 = parts[0];
              var types = (0, service_types_1.toType)(parts.slice(1, -1).join("."));
              service3.name = name2;
              service3.fqdn = rr.name;
              service3.host = rr.data.target;
              service3.referer = referer;
              service3.port = rr.data.port;
              service3.type = types.name;
              service3.protocol = types.protocol;
            } else if (rr.type === "TXT") {
              service3.rawTxt = rr.data;
              service3.txt = this.txt.decodeAll(rr.data);
            }
          });
          if (!service3.name)
            return;
          records.filter((rr) => (rr.type === "A" || rr.type === "AAAA") && (0, dns_equal_1.default)(rr.name, service3.host)).forEach((rr) => service3.addresses.push(rr.data));
          return service3;
        }).filter((rr) => !!rr);
      }
    };
    exports.Browser = Browser2;
    exports.default = Browser2;
  }
});

// node_modules/bonjour-service/dist/index.js
var require_dist = __commonJS({
  "node_modules/bonjour-service/dist/index.js"(exports) {
    "use strict";
    var __importDefault = exports && exports.__importDefault || function(mod) {
      return mod && mod.__esModule ? mod : { "default": mod };
    };
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Browser = exports.Service = exports.Bonjour = void 0;
    var registry_1 = __importDefault(require_registry());
    var mdns_server_1 = __importDefault(require_mdns_server());
    var browser_1 = __importDefault(require_browser());
    exports.Browser = browser_1.default;
    var service_1 = __importDefault(require_service());
    exports.Service = service_1.default;
    var Bonjour2 = class {
      constructor(opts = {}, errorCallback) {
        this.server = new mdns_server_1.default(opts, errorCallback);
        this.registry = new registry_1.default(this.server);
      }
      publish(opts) {
        return this.registry.publish(opts);
      }
      unpublishAll(callback3) {
        return this.registry.unpublishAll(callback3);
      }
      find(opts = null, onup) {
        return new browser_1.default(this.server.mdns, opts, onup);
      }
      findOne(opts = null, timeout3 = 1e4, callback3) {
        const browser2 = new browser_1.default(this.server.mdns, opts);
        var timer;
        browser2.once("up", (service3) => {
          if (timer !== void 0)
            clearTimeout(timer);
          browser2.stop();
          if (callback3)
            callback3(service3);
        });
        timer = setTimeout(() => {
          browser2.stop();
          if (callback3)
            callback3(null);
        }, timeout3);
        return browser2;
      }
      destroy(callback3) {
        this.registry.destroy();
        this.server.mdns.destroy(callback3);
      }
    };
    exports.Bonjour = Bonjour2;
    exports.default = Bonjour2;
  }
});

// src/opencode-bridge.ts
var opencode_bridge_exports = {};
__export(opencode_bridge_exports, {
  mapOpenCodeEventToGravity: () => mapOpenCodeEventToGravity,
  respondToPermission: () => respondToPermission,
  respondToQuestion: () => respondToQuestion
});
import { createConnection as createConnection3 } from "net";
import { appendFileSync as appendFileSync4 } from "fs";
import { join as join6 } from "path";
function log3(msg, level = "debug") {
  if (LOG_LEVELS2[level] < LOG_LEVELS2[CURRENT_LOG_LEVEL]) return;
  try {
    const timestamp = (/* @__PURE__ */ new Date()).toISOString();
    appendFileSync4("/tmp/opencode-bridge.log", `[${timestamp}] [${level.toUpperCase()}] ${msg}
`);
  } catch (e) {
  }
}
function getSocketPath() {
  const gravitySock = process.env.CLAUDE_GRAVITY_SOCK;
  if (gravitySock) {
    return gravitySock;
  }
  const sockDir = process.env.CLAUDE_GRAVITY_SOCK_DIR;
  if (sockDir) {
    return join6(sockDir, "claude-gravity.sock");
  }
  const home = process.env.HOME || "/tmp";
  return join6(home, ".local", "state", "claude-gravity.sock");
}
async function sendToEmacs(eventName, sessionId, cwd, payload, instancePort, instanceDir) {
  const socketPath = getSocketPath();
  log3(`Sending event: ${eventName} session: ${sessionId} to ${socketPath}`);
  return new Promise((resolve) => {
    const client = createConnection3(socketPath);
    client.on("connect", () => {
      log3("Connected to socket");
      const msg = {
        event: eventName,
        session_id: sessionId,
        cwd,
        source: "opencode",
        data: {
          ...payload,
          instance_port: instancePort,
          instance_dir: instanceDir
        }
      };
      log3(`Full message: ${JSON.stringify(msg)}`, "info");
      const message = JSON.stringify(msg) + "\n";
      client.write(message);
      client.end();
    });
    client.on("error", (err) => {
      log3(`Socket error: ${err.message}`, "error");
      resolve();
    });
    client.on("close", () => {
      resolve();
    });
  });
}
async function sendBidirectional(eventName, sessionId, cwd, payload, instancePort, instanceDir) {
  const socketPath = getSocketPath();
  log3(`Sending bidirectional event: ${eventName} session: ${sessionId} to ${socketPath}`);
  return new Promise((resolve) => {
    const client = createConnection3(socketPath);
    let responseBuffer = "";
    let resolved = false;
    const finish = (result3) => {
      if (!resolved) {
        resolved = true;
        resolve(result3);
        client.end();
      }
    };
    client.on("connect", () => {
      log3("Connected to socket (bidirectional)");
      const msg = {
        event: eventName,
        session_id: sessionId,
        cwd,
        source: "opencode",
        needs_response: true,
        data: {
          ...payload,
          instance_port: instancePort,
          instance_dir: instanceDir
        }
      };
      log3(`Full message (bidirectional): ${JSON.stringify(msg)}`, "info");
      client.write(JSON.stringify(msg) + "\n");
    });
    client.on("data", (chunk) => {
      responseBuffer += chunk.toString();
      const newlineIdx = responseBuffer.indexOf("\n");
      if (newlineIdx >= 0) {
        const line = responseBuffer.substring(0, newlineIdx);
        try {
          const response = JSON.parse(line);
          log3(`Received bidirectional response: ${JSON.stringify(response)}`, "info");
          finish(response);
        } catch (e) {
          log3(`Failed to parse bidirectional response: ${e}`, "error");
          finish(null);
        }
      }
    });
    client.on("error", (err) => {
      log3(`Socket error (bidirectional): ${err.message}`, "error");
      finish(null);
    });
    client.on("close", () => {
      finish(null);
    });
    setTimeout(() => {
      log3(`Bidirectional timeout for ${eventName}`, "warn");
      finish(null);
    }, 96 * 60 * 60 * 1e3);
  });
}
async function respondToPermission(port, directory, permissionId, emacsResponse) {
  const decision = emacsResponse?.hookSpecificOutput?.decision;
  if (!decision) {
    log3("No decision in Emacs response for permission", "warn");
    return;
  }
  const behavior = decision.behavior;
  const updatedPerms = decision.updatedPermissions;
  let ocResponse;
  if (behavior === "allow") {
    ocResponse = updatedPerms ? "always" : "once";
  } else {
    ocResponse = "reject";
  }
  try {
    const url = `http://localhost:${port}/permission/${encodeURIComponent(permissionId)}/reply?directory=${encodeURIComponent(directory)}`;
    log3(`Posting permission reply: ${ocResponse} to ${url}`, "info");
    const response = await fetch(url, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(ocResponse)
    });
    if (!response.ok) {
      log3(`Permission reply failed: ${response.status} ${response.statusText}`, "error");
    } else {
      log3(`Permission ${permissionId} \u2192 ${ocResponse}`, "info");
    }
  } catch (e) {
    log3(`Failed to post permission reply: ${e}`, "error");
  }
}
async function respondToQuestion(port, directory, questionId, emacsResponse) {
  const answer = emacsResponse?.answer;
  const hookOutput = emacsResponse?.hookSpecificOutput;
  if (!answer && hookOutput) {
    try {
      const url = `http://localhost:${port}/question/${encodeURIComponent(questionId)}/reject?directory=${encodeURIComponent(directory)}`;
      log3(`Posting question reject to ${url}`, "info");
      const response = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" }
      });
      if (!response.ok) {
        log3(`Question reject failed: ${response.status} ${response.statusText}`, "error");
      }
    } catch (e) {
      log3(`Failed to post question reject: ${e}`, "error");
    }
    return;
  }
  if (answer) {
    try {
      const url = `http://localhost:${port}/question/${encodeURIComponent(questionId)}/reply?directory=${encodeURIComponent(directory)}`;
      const answersArray = emacsResponse?.answers || [answer];
      const flatAnswers = answersArray.map(
        (a) => Array.isArray(a) ? a.join(", ") : typeof a === "string" ? a : String(a)
      );
      log3(`Posting question reply: ${JSON.stringify(flatAnswers)} to ${url}`, "info");
      const response = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ answers: flatAnswers })
      });
      if (!response.ok) {
        log3(`Question reply failed: ${response.status} ${response.statusText}`, "error");
      } else {
        log3(`Question ${questionId} \u2192 ${JSON.stringify(flatAnswers)}`, "info");
      }
    } catch (e) {
      log3(`Failed to post question reply: ${e}`, "error");
    }
  }
}
async function fetchJson(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${response.statusText}`);
  }
  return response.json();
}
async function getSessionList(port, directory) {
  try {
    const url = `http://localhost:${port}/session?directory=${encodeURIComponent(directory)}`;
    return await fetchJson(url);
  } catch (e) {
    log3(`Failed to fetch sessions from port ${port}: ${e}`, "warn");
    return [];
  }
}
async function getSessionStatus(port, directory) {
  try {
    const url = `http://localhost:${port}/session/status?directory=${encodeURIComponent(directory)}`;
    return await fetchJson(url);
  } catch (e) {
    log3(`Failed to fetch session status from port ${port}: ${e}`, "warn");
    return {};
  }
}
async function getVcsInfo(port, directory) {
  try {
    const url = `http://localhost:${port}/vcs?directory=${encodeURIComponent(directory)}`;
    return await fetchJson(url);
  } catch (e) {
    log3(`Failed to fetch VCS info from port ${port}: ${e}`, "warn");
    return {};
  }
}
function mapOpenCodeEventToGravity(event, instancePort, instanceDir) {
  const type = event.type;
  if (!type) return null;
  const props = event.properties || {};
  switch (type) {
    case "session.created": {
      const session = props.info;
      if (!session) return null;
      return {
        event: "SessionStart",
        sessionId: session.id,
        cwd: session.directory,
        data: {
          slug: session.slug,
          title: session.title,
          parent_id: session.parentID,
          project_id: session.projectID,
          time_created: session.time?.created,
          permission: session.permission
        }
      };
    }
    case "session.deleted": {
      const sessionId = props.sessionID;
      return {
        event: "SessionEnd",
        sessionId,
        cwd: instanceDir,
        data: {}
      };
    }
    case "session.status": {
      const sessionId = props.sessionID;
      const status = props.status;
      return {
        event: "SessionStatus",
        sessionId,
        cwd: instanceDir,
        data: {
          status: status.type,
          attempt: status.attempt,
          message: status.message
        }
      };
    }
    case "session.idle": {
      const sessionId = props.sessionID;
      return {
        event: "SessionIdle",
        sessionId,
        cwd: instanceDir,
        data: {}
      };
    }
    case "message.updated": {
      const message = props.info;
      if (!message) return null;
      const sessionId = message.sessionID;
      const messageId = message.id;
      if (message.role) {
        messageRoles.set(messageId, message.role);
      }
      if (message.role === "user") {
        return {
          event: "UserPromptSubmit",
          sessionId,
          cwd: instanceDir,
          data: {
            message_id: messageId,
            agent: message.agent,
            model: message.model,
            tools: message.tools,
            system: message.system
          }
        };
      } else if (message.role === "assistant") {
        return {
          event: "AssistantMessage",
          sessionId,
          cwd: instanceDir,
          data: {
            message_id: messageId,
            parent_id: message.parentID,
            model_id: message.modelID,
            provider_id: message.providerID,
            cost: message.cost,
            tokens: message.tokens,
            finish: message.finish,
            error: message.error
          }
        };
      }
      return null;
    }
    case "message.part.updated": {
      const part = props.part;
      if (!part) return null;
      const sessionId = part.sessionID;
      const messageId = part.messageID;
      const messageRole = messageRoles.get(messageId);
      return {
        event: "MessagePart",
        sessionId,
        cwd: instanceDir,
        data: {
          message_id: messageId,
          message_role: messageRole,
          part_id: part.id,
          part_type: part.type,
          text: part.text,
          tool: part.type === "tool" ? {
            call_id: part.callID,
            tool_name: part.tool,
            state: part.state
          } : void 0,
          delta: props.delta
        }
      };
    }
    case "permission.asked": {
      return {
        event: "PermissionRequest",
        sessionId: props.sessionID,
        cwd: instanceDir,
        data: {
          permission_id: props.id,
          // Flattened fields for Emacs dispatch compatibility (expects tool_name/tool_input at root)
          tool_name: props.tool?.name,
          tool_input: props.tool?.input,
          permission: props.permission,
          patterns: props.patterns,
          metadata: props.metadata,
          always: props.always,
          tool: props.tool
        }
      };
    }
    case "question.asked": {
      return {
        event: "AskUserQuestion",
        sessionId: props.sessionID,
        cwd: instanceDir,
        data: {
          question_id: props.id,
          // Flattened fields for Emacs dispatch compatibility
          tool_name: "AskUserQuestion",
          tool_input: { questions: props.questions },
          questions: props.questions,
          tool: props.tool
        }
      };
    }
    case "vcs.branch.updated": {
      const sessionId = props.sessionID;
      return {
        event: "VcsBranchUpdate",
        sessionId: sessionId || "unknown",
        cwd: instanceDir,
        data: {
          branch: props.branch
        }
      };
    }
    case "session.updated": {
      const session = props.info;
      if (!session) return null;
      return {
        event: "SessionUpdate",
        sessionId: session.id,
        cwd: instanceDir,
        data: {
          title: session.title,
          slug: session.slug,
          summary: session.summary
        }
      };
    }
    default:
      log3(`Unhandled event type: ${type}`, "debug");
      return null;
  }
}
async function subscribeToEvents(instance) {
  const { port, directory } = instance;
  try {
    const url = `http://localhost:${port}/global/event?directory=${encodeURIComponent(directory)}`;
    log3(`Subscribing to events from port ${port}`);
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to connect: ${response.status}`);
    }
    if (!response.body) {
      throw new Error("No response body");
    }
    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let buffer = "";
    instance.abortController = new AbortController();
    const signal = instance.abortController.signal;
    signal.addEventListener("abort", () => {
      reader.cancel();
    });
    while (true) {
      if (signal.aborted) break;
      const { done: done4, value } = await reader.read();
      if (done4) break;
      buffer += decoder.decode(value, { stream: true });
      const lines = buffer.split("\n");
      buffer = lines.pop() || "";
      for (const line of lines) {
        if (!line.trim() || !line.startsWith("data: ")) continue;
        try {
          const data = line.slice(6);
          if (data === "") continue;
          const wrapper = JSON.parse(data);
          const event = wrapper.payload || wrapper;
          log3(`Received event: ${event.type} for port ${port}`, "info");
          const mapped = mapOpenCodeEventToGravity(event, port, directory);
          if (mapped) {
            if (mapped.event === "PermissionRequest" || mapped.event === "AskUserQuestion") {
              log3(`Sending bidirectional event: ${mapped.event} for session: ${mapped.sessionId}`, "info");
              const emacsResponse = await sendBidirectional(
                mapped.event,
                mapped.sessionId,
                mapped.cwd,
                mapped.data,
                port,
                directory
              );
              if (emacsResponse) {
                if (mapped.event === "PermissionRequest") {
                  await respondToPermission(port, directory, mapped.data.permission_id, emacsResponse);
                } else {
                  await respondToQuestion(port, directory, mapped.data.question_id, emacsResponse);
                }
              } else {
                log3(`No response from Emacs for ${mapped.event}`, "warn");
              }
            } else {
              log3(`Sending event: ${mapped.event} for session: ${mapped.sessionId}`, "info");
              await sendToEmacs(mapped.event, mapped.sessionId, mapped.cwd, mapped.data, port, directory);
            }
          } else {
            log3(`No mapping for event: ${event.type}`, "debug");
          }
        } catch (e) {
          log3(`Failed to parse event: ${e}`, "warn");
        }
      }
    }
  } catch (e) {
    if (e.name !== "AbortError") {
      log3(`Event subscription error for port ${port}: ${e}`, "error");
    }
  }
  instances.delete(port);
  log3(`Disconnected from port ${port}`, "info");
}
async function pollSessions(instance) {
  const { port, directory } = instance;
  try {
    const sessions = await getSessionList(port, directory);
    const statusMap = await getSessionStatus(port, directory);
    const vcs = await getVcsInfo(port, directory);
    for (const session of sessions) {
      const status = statusMap[session.id];
      await sendToEmacs("SessionStatus", session.id, directory, {
        status: status?.type || "idle",
        title: session.title,
        slug: session.slug,
        branch: vcs.branch
      }, port, directory);
    }
    if (vcs.branch) {
      const mainSession = sessions[0];
      if (mainSession) {
        await sendToEmacs("VcsBranchUpdate", mainSession.id, directory, { branch: vcs.branch }, port, directory);
      }
    }
  } catch (e) {
    log3(`Polling error for port ${port}: ${e}`, "warn");
  }
}
async function startInstance(port, directory) {
  if (instances.has(port)) {
    log3(`Already connected to port ${port}`, "debug");
    return;
  }
  log3(`Connecting to OpenCode instance on port ${port}, directory: ${directory}`, "info");
  const instance = {
    port,
    directory,
    sessions: /* @__PURE__ */ new Map(),
    abortController: null
  };
  instances.set(port, instance);
  subscribeToEvents(instance);
  setInterval(async () => {
    await pollSessions(instance);
  }, 5e3);
  await pollSessions(instance);
}
function startDiscovery() {
  log3("Starting mDNS discovery for OpenCode instances", "info");
  bonjour = new import_bonjour_service.default();
  browser = bonjour.find({ type: "http" });
  browser.on("up", (service3) => {
    const name = service3.name;
    if (!name.startsWith("opencode-")) {
      return;
    }
    const port = service3.port;
    let directory = "/";
    if (service3.txt && service3.txt.path) {
      directory = service3.txt.path;
    }
    log3(`Discovered OpenCode instance: ${name} on port ${port}`, "info");
    startInstance(port, directory);
  });
  browser.on("down", (service3) => {
    const port = service3.port;
    const instance = instances.get(port);
    if (instance && instance.abortController) {
      instance.abortController.abort();
    }
    instances.delete(port);
    log3(`OpenCode instance removed: port ${port}`, "info");
  });
}
async function main() {
  log3("Starting OpenCode bridge", "info");
  log3(`Socket path: ${getSocketPath()}`, "info");
  startDiscovery();
  setInterval(() => {
    log3(`Active instances: ${instances.size}`, "debug");
  }, 3e4);
}
var import_bonjour_service, LOG_LEVELS2, CURRENT_LOG_LEVEL, instances, bonjour, browser, messageRoles;
var init_opencode_bridge = __esm({
  "src/opencode-bridge.ts"() {
    "use strict";
    import_bonjour_service = __toESM(require_dist(), 1);
    LOG_LEVELS2 = { debug: 0, info: 1, warn: 2, error: 3 };
    CURRENT_LOG_LEVEL = process.env.EMACS_BRIDGE_LOG_LEVEL || "info";
    instances = /* @__PURE__ */ new Map();
    bonjour = null;
    browser = null;
    messageRoles = /* @__PURE__ */ new Map();
    main().catch((e) => {
      log3(`Fatal error: ${e}`, "error");
      process.exit(1);
    });
  }
});

// ../../node_modules/effect/dist/Pipeable.js
var pipeArguments = (self, args3) => {
  switch (args3.length) {
    case 0:
      return self;
    case 1:
      return args3[0](self);
    case 2:
      return args3[1](args3[0](self));
    case 3:
      return args3[2](args3[1](args3[0](self)));
    case 4:
      return args3[3](args3[2](args3[1](args3[0](self))));
    case 5:
      return args3[4](args3[3](args3[2](args3[1](args3[0](self)))));
    case 6:
      return args3[5](args3[4](args3[3](args3[2](args3[1](args3[0](self))))));
    case 7:
      return args3[6](args3[5](args3[4](args3[3](args3[2](args3[1](args3[0](self)))))));
    case 8:
      return args3[7](args3[6](args3[5](args3[4](args3[3](args3[2](args3[1](args3[0](self))))))));
    case 9:
      return args3[8](args3[7](args3[6](args3[5](args3[4](args3[3](args3[2](args3[1](args3[0](self)))))))));
    default: {
      let ret = self;
      for (let i = 0, len = args3.length; i < len; i++) {
        ret = args3[i](ret);
      }
      return ret;
    }
  }
};
var Prototype = {
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var Class = /* @__PURE__ */ (function() {
  function PipeableBase() {
  }
  PipeableBase.prototype = Prototype;
  return PipeableBase;
})();

// ../../node_modules/effect/dist/Function.js
var dual = function(arity, body) {
  if (typeof arity === "function") {
    return function() {
      return arity(arguments) ? body.apply(this, arguments) : (self) => body(self, ...arguments);
    };
  }
  switch (arity) {
    case 0:
    case 1:
      throw new RangeError(`Invalid arity ${arity}`);
    case 2:
      return function(a, b) {
        if (arguments.length >= 2) {
          return body(a, b);
        }
        return function(self) {
          return body(self, a);
        };
      };
    case 3:
      return function(a, b, c) {
        if (arguments.length >= 3) {
          return body(a, b, c);
        }
        return function(self) {
          return body(self, a, b);
        };
      };
    default:
      return function() {
        if (arguments.length >= arity) {
          return body.apply(this, arguments);
        }
        const args3 = arguments;
        return function(self) {
          return body(self, ...args3);
        };
      };
  }
};
var identity = (a) => a;
var constant = (value) => () => value;
var constTrue = /* @__PURE__ */ constant(true);
var constFalse = /* @__PURE__ */ constant(false);
var constUndefined = /* @__PURE__ */ constant(void 0);
var constVoid = constUndefined;
function pipe(a, ...args3) {
  return pipeArguments(a, args3);
}

// ../../node_modules/effect/dist/internal/equal.js
var getAllObjectKeys = (obj) => {
  const keys = new Set(Reflect.ownKeys(obj));
  if (obj.constructor === Object) return keys;
  if (obj instanceof Error) {
    keys.delete("stack");
  }
  const proto = Object.getPrototypeOf(obj);
  let current = proto;
  while (current !== null && current !== Object.prototype) {
    const ownKeys = Reflect.ownKeys(current);
    for (let i = 0; i < ownKeys.length; i++) {
      keys.add(ownKeys[i]);
    }
    current = Object.getPrototypeOf(current);
  }
  if (keys.has("constructor") && typeof obj.constructor === "function" && proto === obj.constructor.prototype) {
    keys.delete("constructor");
  }
  return keys;
};
var byReferenceInstances = /* @__PURE__ */ new WeakSet();

// ../../node_modules/effect/dist/Predicate.js
function isString(input) {
  return typeof input === "string";
}
function isNumber(input) {
  return typeof input === "number";
}
function isBigInt(input) {
  return typeof input === "bigint";
}
function isFunction(input) {
  return typeof input === "function";
}
function isNotUndefined(input) {
  return input !== void 0;
}
function isObjectKeyword(input) {
  return typeof input === "object" && input !== null || isFunction(input);
}
var hasProperty = /* @__PURE__ */ dual(2, (self, property) => isObjectKeyword(self) && property in self);
var isTagged = /* @__PURE__ */ dual(2, (self, tag) => hasProperty(self, "_tag") && self["_tag"] === tag);
function isIterable(input) {
  return hasProperty(input, Symbol.iterator) || isString(input);
}

// ../../node_modules/effect/dist/Hash.js
var symbol = "~effect/interfaces/Hash";
var hash = (self) => {
  switch (typeof self) {
    case "number":
      return number(self);
    case "bigint":
      return string(self.toString(10));
    case "boolean":
      return string(String(self));
    case "symbol":
      return string(String(self));
    case "string":
      return string(self);
    case "undefined":
      return string("undefined");
    case "function":
    case "object": {
      if (self === null) {
        return string("null");
      } else if (self instanceof Date) {
        return string(self.toISOString());
      } else if (self instanceof RegExp) {
        return string(self.toString());
      } else {
        if (byReferenceInstances.has(self)) {
          return random(self);
        }
        if (hashCache.has(self)) {
          return hashCache.get(self);
        }
        const h = withVisitedTracking(self, () => {
          if (isHash(self)) {
            return self[symbol]();
          } else if (typeof self === "function") {
            return random(self);
          } else if (Array.isArray(self)) {
            return array(self);
          } else if (self instanceof Map) {
            return hashMap(self);
          } else if (self instanceof Set) {
            return hashSet(self);
          }
          return structure(self);
        });
        hashCache.set(self, h);
        return h;
      }
    }
    default:
      throw new Error(`BUG: unhandled typeof ${typeof self} - please report an issue at https://github.com/Effect-TS/effect/issues`);
  }
};
var random = (self) => {
  if (!randomHashCache.has(self)) {
    randomHashCache.set(self, number(Math.floor(Math.random() * Number.MAX_SAFE_INTEGER)));
  }
  return randomHashCache.get(self);
};
var combine = /* @__PURE__ */ dual(2, (self, b) => self * 53 ^ b);
var optimize = (n) => n & 3221225471 | n >>> 1 & 1073741824;
var isHash = (u) => hasProperty(u, symbol);
var number = (n) => {
  if (n !== n) {
    return string("NaN");
  }
  if (n === Infinity) {
    return string("Infinity");
  }
  if (n === -Infinity) {
    return string("-Infinity");
  }
  let h = n | 0;
  if (h !== n) {
    h ^= n * 4294967295;
  }
  while (n > 4294967295) {
    h ^= n /= 4294967295;
  }
  return optimize(h);
};
var string = (str) => {
  let h = 5381, i = str.length;
  while (i) {
    h = h * 33 ^ str.charCodeAt(--i);
  }
  return optimize(h);
};
var structureKeys = (o, keys) => {
  let h = 12289;
  for (const key of keys) {
    h ^= combine(hash(key), hash(o[key]));
  }
  return optimize(h);
};
var structure = (o) => structureKeys(o, getAllObjectKeys(o));
var iterableWith = (seed, f) => (iter) => {
  let h = seed;
  for (const element of iter) {
    h ^= f(element);
  }
  return optimize(h);
};
var array = /* @__PURE__ */ iterableWith(6151, hash);
var hashMap = /* @__PURE__ */ iterableWith(/* @__PURE__ */ string("Map"), ([k, v]) => combine(hash(k), hash(v)));
var hashSet = /* @__PURE__ */ iterableWith(/* @__PURE__ */ string("Set"), hash);
var randomHashCache = /* @__PURE__ */ new WeakMap();
var hashCache = /* @__PURE__ */ new WeakMap();
var visitedObjects = /* @__PURE__ */ new WeakSet();
function withVisitedTracking(obj, fn3) {
  if (visitedObjects.has(obj)) {
    return string("[Circular]");
  }
  visitedObjects.add(obj);
  const result3 = fn3();
  visitedObjects.delete(obj);
  return result3;
}

// ../../node_modules/effect/dist/Equal.js
var symbol2 = "~effect/interfaces/Equal";
function equals() {
  if (arguments.length === 1) {
    return (self) => compareBoth(self, arguments[0]);
  }
  return compareBoth(arguments[0], arguments[1]);
}
function compareBoth(self, that) {
  if (self === that) return true;
  if (self == null || that == null) return false;
  const selfType = typeof self;
  if (selfType !== typeof that) {
    return false;
  }
  if (selfType === "number" && self !== self && that !== that) {
    return true;
  }
  if (selfType !== "object" && selfType !== "function") {
    return false;
  }
  if (byReferenceInstances.has(self) || byReferenceInstances.has(that)) {
    return false;
  }
  return withCache(self, that, compareObjects);
}
function withVisitedTracking2(self, that, fn3) {
  const hasLeft = visitedLeft.has(self);
  const hasRight = visitedRight.has(that);
  if (hasLeft && hasRight) {
    return true;
  }
  if (hasLeft || hasRight) {
    return false;
  }
  visitedLeft.add(self);
  visitedRight.add(that);
  const result3 = fn3();
  visitedLeft.delete(self);
  visitedRight.delete(that);
  return result3;
}
var visitedLeft = /* @__PURE__ */ new WeakSet();
var visitedRight = /* @__PURE__ */ new WeakSet();
function compareObjects(self, that) {
  if (hash(self) !== hash(that)) {
    return false;
  } else if (self instanceof Date) {
    if (!(that instanceof Date)) return false;
    return self.toISOString() === that.toISOString();
  } else if (self instanceof RegExp) {
    if (!(that instanceof RegExp)) return false;
    return self.toString() === that.toString();
  }
  const selfIsEqual = isEqual(self);
  const thatIsEqual = isEqual(that);
  if (selfIsEqual !== thatIsEqual) return false;
  const bothEquals = selfIsEqual && thatIsEqual;
  if (typeof self === "function" && !bothEquals) {
    return false;
  }
  return withVisitedTracking2(self, that, () => {
    if (bothEquals) {
      return self[symbol2](that);
    } else if (Array.isArray(self)) {
      if (!Array.isArray(that) || self.length !== that.length) {
        return false;
      }
      return compareArrays(self, that);
    } else if (self instanceof Map) {
      if (!(that instanceof Map) || self.size !== that.size) {
        return false;
      }
      return compareMaps(self, that);
    } else if (self instanceof Set) {
      if (!(that instanceof Set) || self.size !== that.size) {
        return false;
      }
      return compareSets(self, that);
    }
    return compareRecords(self, that);
  });
}
function withCache(self, that, f) {
  let selfMap = equalityCache.get(self);
  if (!selfMap) {
    selfMap = /* @__PURE__ */ new WeakMap();
    equalityCache.set(self, selfMap);
  } else if (selfMap.has(that)) {
    return selfMap.get(that);
  }
  const result3 = f(self, that);
  selfMap.set(that, result3);
  let thatMap = equalityCache.get(that);
  if (!thatMap) {
    thatMap = /* @__PURE__ */ new WeakMap();
    equalityCache.set(that, thatMap);
  }
  thatMap.set(self, result3);
  return result3;
}
var equalityCache = /* @__PURE__ */ new WeakMap();
function compareArrays(self, that) {
  for (let i = 0; i < self.length; i++) {
    if (!compareBoth(self[i], that[i])) {
      return false;
    }
  }
  return true;
}
function compareRecords(self, that) {
  const selfKeys = getAllObjectKeys(self);
  const thatKeys = getAllObjectKeys(that);
  if (selfKeys.size !== thatKeys.size) {
    return false;
  }
  for (const key of selfKeys) {
    if (!thatKeys.has(key) || !compareBoth(self[key], that[key])) {
      return false;
    }
  }
  return true;
}
function makeCompareMap(keyEquivalence, valueEquivalence) {
  return function compareMaps2(self, that) {
    for (const [selfKey, selfValue] of self) {
      let found = false;
      for (const [thatKey, thatValue] of that) {
        if (keyEquivalence(selfKey, thatKey) && valueEquivalence(selfValue, thatValue)) {
          found = true;
          break;
        }
      }
      if (!found) {
        return false;
      }
    }
    return true;
  };
}
var compareMaps = /* @__PURE__ */ makeCompareMap(compareBoth, compareBoth);
function makeCompareSet(equivalence) {
  return function compareSets2(self, that) {
    for (const selfValue of self) {
      let found = false;
      for (const thatValue of that) {
        if (equivalence(selfValue, thatValue)) {
          found = true;
          break;
        }
      }
      if (!found) {
        return false;
      }
    }
    return true;
  };
}
var compareSets = /* @__PURE__ */ makeCompareSet(compareBoth);
var isEqual = (u) => hasProperty(u, symbol2);
var asEquivalence = () => equals;
var byReferenceUnsafe = (obj) => {
  byReferenceInstances.add(obj);
  return obj;
};

// ../../node_modules/effect/dist/Redactable.js
var symbolRedactable = /* @__PURE__ */ Symbol.for("~effect/Inspectable/redactable");
var isRedactable = (u) => hasProperty(u, symbolRedactable);
function redact(u) {
  if (isRedactable(u)) return getRedacted(u);
  return u;
}
function getRedacted(redactable) {
  return redactable[symbolRedactable](globalThis[currentFiberTypeId]?.services ?? emptyServiceMap);
}
var currentFiberTypeId = "~effect/Fiber/currentFiber";
var emptyServiceMap = {
  "~effect/ServiceMap": {},
  mapUnsafe: /* @__PURE__ */ new Map(),
  pipe() {
    return pipeArguments(this, arguments);
  }
};

// ../../node_modules/effect/dist/Formatter.js
function format(input, options) {
  const space = options?.space ?? 0;
  const seen = /* @__PURE__ */ new WeakSet();
  const gap = !space ? "" : typeof space === "number" ? " ".repeat(space) : space;
  const ind = (d) => gap.repeat(d);
  const wrap = (v, body) => {
    const ctor = v?.constructor;
    return ctor && ctor !== Object.prototype.constructor && ctor.name ? `${ctor.name}(${body})` : body;
  };
  const ownKeys = (o) => {
    try {
      return Reflect.ownKeys(o);
    } catch {
      return ["[ownKeys threw]"];
    }
  };
  function recur(v, d = 0) {
    if (Array.isArray(v)) {
      if (seen.has(v)) return CIRCULAR;
      seen.add(v);
      if (!gap || v.length <= 1) return `[${v.map((x) => recur(x, d)).join(",")}]`;
      const inner = v.map((x) => recur(x, d + 1)).join(",\n" + ind(d + 1));
      return `[
${ind(d + 1)}${inner}
${ind(d)}]`;
    }
    if (v instanceof Date) return formatDate(v);
    if (!options?.ignoreToString && hasProperty(v, "toString") && typeof v["toString"] === "function" && v["toString"] !== Object.prototype.toString && v["toString"] !== Array.prototype.toString) {
      const s = safeToString(v);
      if (v instanceof Error && v.cause) {
        return `${s} (cause: ${recur(v.cause, d)})`;
      }
      return s;
    }
    if (typeof v === "string") return JSON.stringify(v);
    if (typeof v === "number" || v == null || typeof v === "boolean" || typeof v === "symbol") return String(v);
    if (typeof v === "bigint") return String(v) + "n";
    if (typeof v === "object" || typeof v === "function") {
      if (seen.has(v)) return CIRCULAR;
      seen.add(v);
      if (symbolRedactable in v) return format(getRedacted(v));
      if (Symbol.iterator in v) {
        return `${v.constructor.name}(${recur(Array.from(v), d)})`;
      }
      const keys = ownKeys(v);
      if (!gap || keys.length <= 1) {
        const body2 = `{${keys.map((k) => `${formatPropertyKey(k)}:${recur(v[k], d)}`).join(",")}}`;
        return wrap(v, body2);
      }
      const body = `{
${keys.map((k) => `${ind(d + 1)}${formatPropertyKey(k)}: ${recur(v[k], d + 1)}`).join(",\n")}
${ind(d)}}`;
      return wrap(v, body);
    }
    return String(v);
  }
  return recur(input, 0);
}
var CIRCULAR = "[Circular]";
function formatPropertyKey(name) {
  return typeof name === "string" ? JSON.stringify(name) : String(name);
}
function formatDate(date) {
  try {
    return date.toISOString();
  } catch {
    return "Invalid Date";
  }
}
function safeToString(input) {
  try {
    const s = input.toString();
    return typeof s === "string" ? s : String(s);
  } catch {
    return "[toString threw]";
  }
}
function formatJson(input, options) {
  let cache = [];
  const out = JSON.stringify(input, (_key, value) => typeof value === "object" && value !== null ? cache.includes(value) ? void 0 : cache.push(value) && redact(value) : value, options?.space);
  cache = void 0;
  return out;
}

// ../../node_modules/effect/dist/Inspectable.js
var NodeInspectSymbol = /* @__PURE__ */ Symbol.for("nodejs.util.inspect.custom");
var toJson = (input) => {
  try {
    if (hasProperty(input, "toJSON") && isFunction(input["toJSON"]) && input["toJSON"].length === 0) {
      return input.toJSON();
    } else if (Array.isArray(input)) {
      return input.map(toJson);
    }
  } catch {
    return "[toJSON threw]";
  }
  return redact(input);
};
var toStringUnknown = (u, whitespace = 2) => {
  if (typeof u === "string") {
    return u;
  }
  try {
    return typeof u === "object" ? stringifyCircular(u, whitespace) : String(u);
  } catch {
    return String(u);
  }
};
var stringifyCircular = (obj, whitespace) => {
  let cache = [];
  const retVal = JSON.stringify(obj, (_key, value) => typeof value === "object" && value !== null ? cache.includes(value) ? void 0 : cache.push(value) && redact(value) : value, whitespace);
  cache = void 0;
  return retVal;
};
var BaseProto = {
  toJSON() {
    return toJson(this);
  },
  [NodeInspectSymbol]() {
    return this.toJSON();
  },
  toString() {
    return format(this.toJSON());
  }
};
var Class2 = class {
  /**
   * Node.js custom inspection method.
   *
   * @since 2.0.0
   */
  [NodeInspectSymbol]() {
    return this.toJSON();
  }
  /**
   * Returns a formatted string representation of this object.
   *
   * @since 2.0.0
   */
  toString() {
    return format(this.toJSON());
  }
};

// ../../node_modules/effect/dist/Utils.js
var GenKindTypeId = "~effect/Utils/GenKind";
var GenKindImpl = class {
  value;
  constructor(value) {
    this.value = value;
  }
  get _F() {
    return identity;
  }
  get _R() {
    return (_) => _;
  }
  get _O() {
    return (_) => _;
  }
  get _E() {
    return (_) => _;
  }
  [GenKindTypeId] = GenKindTypeId;
  [Symbol.iterator]() {
    return new SingleShotGen(this);
  }
};
var SingleShotGen = class _SingleShotGen {
  called = false;
  self;
  constructor(self) {
    this.self = self;
  }
  /**
   * @since 2.0.0
   */
  next(a) {
    return this.called ? {
      value: a,
      done: true
    } : (this.called = true, {
      value: this.self,
      done: false
    });
  }
  /**
   * @since 2.0.0
   */
  [Symbol.iterator]() {
    return new _SingleShotGen(this.self);
  }
};
var InternalTypeId = "~effect/Effect/internal";
var standard = {
  [InternalTypeId]: (body) => {
    return body();
  }
};
var forced = {
  [InternalTypeId]: (body) => {
    try {
      return body();
    } finally {
    }
  }
};
var isNotOptimizedAway = /* @__PURE__ */ standard[InternalTypeId](() => new Error().stack)?.includes(InternalTypeId) === true;
var internalCall = isNotOptimizedAway ? standard[InternalTypeId] : forced[InternalTypeId];
var genConstructor = function* () {
}.constructor;

// ../../node_modules/effect/dist/internal/core.js
var EffectTypeId = `~effect/Effect`;
var ExitTypeId = `~effect/Exit`;
var effectVariance = {
  _A: identity,
  _E: identity,
  _R: identity
};
var identifier = `${EffectTypeId}/identifier`;
var args = `${EffectTypeId}/args`;
var evaluate = `${EffectTypeId}/evaluate`;
var contA = `${EffectTypeId}/successCont`;
var contE = `${EffectTypeId}/failureCont`;
var contAll = `${EffectTypeId}/ensureCont`;
var Yield = /* @__PURE__ */ Symbol.for("effect/Effect/Yield");
var PipeInspectableProto = {
  pipe() {
    return pipeArguments(this, arguments);
  },
  toJSON() {
    return {
      ...this
    };
  },
  toString() {
    return format(this.toJSON(), {
      ignoreToString: true,
      space: 2
    });
  },
  [NodeInspectSymbol]() {
    return this.toJSON();
  }
};
var StructuralProto = {
  [symbol]() {
    return structureKeys(this, Object.keys(this));
  },
  [symbol2](that) {
    const selfKeys = Object.keys(this);
    const thatKeys = Object.keys(that);
    if (selfKeys.length !== thatKeys.length) return false;
    for (let i = 0; i < selfKeys.length; i++) {
      if (selfKeys[i] !== thatKeys[i] && !equals(this[selfKeys[i]], that[selfKeys[i]])) {
        return false;
      }
    }
    return true;
  }
};
var YieldableProto = {
  [Symbol.iterator]() {
    return new SingleShotGen(this);
  }
};
var YieldableErrorProto = {
  ...YieldableProto,
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var EffectProto = {
  [EffectTypeId]: effectVariance,
  ...PipeInspectableProto,
  [Symbol.iterator]() {
    return new SingleShotGen(this);
  },
  asEffect() {
    return this;
  },
  toJSON() {
    return {
      _id: "Effect",
      op: this[identifier],
      ...args in this ? {
        args: this[args]
      } : void 0
    };
  }
};
var isEffect = (u) => hasProperty(u, EffectTypeId);
var isExit = (u) => hasProperty(u, ExitTypeId);
var CauseTypeId = "~effect/Cause";
var CauseReasonTypeId = "~effect/Cause/Reason";
var isCause = (self) => hasProperty(self, CauseTypeId);
var CauseImpl = class {
  [CauseTypeId];
  reasons;
  constructor(failures) {
    this[CauseTypeId] = CauseTypeId;
    this.reasons = failures;
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
  toJSON() {
    return {
      _id: "Cause",
      failures: this.reasons.map((f) => f.toJSON())
    };
  }
  toString() {
    return `Cause(${format(this.reasons)})`;
  }
  [NodeInspectSymbol]() {
    return this.toJSON();
  }
  [symbol2](that) {
    return isCause(that) && this.reasons.length === that.reasons.length && this.reasons.every((e, i) => equals(e, that.reasons[i]));
  }
  [symbol]() {
    return array(this.reasons);
  }
};
var annotationsMap = /* @__PURE__ */ new WeakMap();
var ReasonBase = class {
  [CauseReasonTypeId];
  annotations;
  _tag;
  constructor(_tag, annotations, originalError) {
    this[CauseReasonTypeId] = CauseReasonTypeId;
    this._tag = _tag;
    if (annotations !== constEmptyAnnotations && typeof originalError === "object" && originalError !== null && annotations.size > 0) {
      const prevAnnotations = annotationsMap.get(originalError);
      if (prevAnnotations) {
        annotations = new Map([...prevAnnotations, ...annotations]);
      }
      annotationsMap.set(originalError, annotations);
    }
    this.annotations = annotations;
  }
  annotate(annotations, options) {
    if (annotations.mapUnsafe.size === 0) return this;
    const newAnnotations = new Map(this.annotations);
    annotations.mapUnsafe.forEach((value, key) => {
      if (options?.overwrite !== true && newAnnotations.has(key)) return;
      newAnnotations.set(key, value);
    });
    const self = Object.assign(Object.create(Object.getPrototypeOf(this)), this);
    self.annotations = newAnnotations;
    return self;
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
  toString() {
    return format(this);
  }
  [NodeInspectSymbol]() {
    return this.toString();
  }
};
var constEmptyAnnotations = /* @__PURE__ */ new Map();
var Fail = class extends ReasonBase {
  error;
  constructor(error, annotations = constEmptyAnnotations) {
    super("Fail", annotations, error);
    this.error = error;
  }
  toString() {
    return `Fail(${format(this.error)})`;
  }
  toJSON() {
    return {
      _tag: "Fail",
      error: this.error
    };
  }
  [symbol2](that) {
    return isFailReason(that) && equals(this.error, that.error) && equals(this.annotations, that.annotations);
  }
  [symbol]() {
    return combine(string(this._tag))(combine(hash(this.error))(hash(this.annotations)));
  }
};
var causeFromReasons = (reasons) => new CauseImpl(reasons);
var causeEmpty = /* @__PURE__ */ new CauseImpl([]);
var causeFail = (error) => new CauseImpl([new Fail(error)]);
var Die = class extends ReasonBase {
  defect;
  constructor(defect, annotations = constEmptyAnnotations) {
    super("Die", annotations, defect);
    this.defect = defect;
  }
  toString() {
    return `Die(${format(this.defect)})`;
  }
  toJSON() {
    return {
      _tag: "Die",
      defect: this.defect
    };
  }
  [symbol2](that) {
    return isDieReason(that) && equals(this.defect, that.defect) && equals(this.annotations, that.annotations);
  }
  [symbol]() {
    return combine(string(this._tag))(combine(hash(this.defect))(hash(this.annotations)));
  }
};
var causeDie = (defect) => new CauseImpl([new Die(defect)]);
var causeAnnotate = /* @__PURE__ */ dual((args3) => isCause(args3[0]), (self, annotations, options) => {
  if (annotations.mapUnsafe.size === 0) return self;
  return new CauseImpl(self.reasons.map((f) => f.annotate(annotations, options)));
});
var isFailReason = (self) => self._tag === "Fail";
var isDieReason = (self) => self._tag === "Die";
var isInterruptReason = (self) => self._tag === "Interrupt";
function defaultEvaluate(_fiber) {
  return exitDie(`Effect.evaluate: Not implemented`);
}
var makePrimitiveProto = (options) => ({
  ...EffectProto,
  [identifier]: options.op,
  [evaluate]: options[evaluate] ?? defaultEvaluate,
  [contA]: options[contA],
  [contE]: options[contE],
  [contAll]: options[contAll]
});
var makePrimitive = (options) => {
  const Proto3 = makePrimitiveProto(options);
  return function() {
    const self = Object.create(Proto3);
    self[args] = options.single === false ? arguments : arguments[0];
    return self;
  };
};
var makeExit = (options) => {
  const Proto3 = {
    ...makePrimitiveProto(options),
    [ExitTypeId]: ExitTypeId,
    _tag: options.op,
    get [options.prop]() {
      return this[args];
    },
    toString() {
      return `${options.op}(${format(this[args])})`;
    },
    toJSON() {
      return {
        _id: "Exit",
        _tag: options.op,
        [options.prop]: this[args]
      };
    },
    [symbol2](that) {
      return isExit(that) && that._tag === this._tag && equals(this[args], that[args]);
    },
    [symbol]() {
      return combine(string(options.op), hash(this[args]));
    }
  };
  return function(value) {
    const self = Object.create(Proto3);
    self[args] = value;
    return self;
  };
};
var exitSucceed = /* @__PURE__ */ makeExit({
  op: "Success",
  prop: "value",
  [evaluate](fiber3) {
    const cont = fiber3.getCont(contA);
    return cont ? cont[contA](this[args], fiber3, this) : fiber3.yieldWith(this);
  }
});
var StackTraceKey = {
  key: "effect/Cause/StackTrace"
};
var InterruptorStackTrace = {
  key: "effect/Cause/InterruptorStackTrace"
};
var exitFailCause = /* @__PURE__ */ makeExit({
  op: "Failure",
  prop: "cause",
  [evaluate](fiber3) {
    let cause = this[args];
    let annotated = false;
    if (fiber3.currentStackFrame) {
      cause = causeAnnotate(cause, {
        mapUnsafe: /* @__PURE__ */ new Map([[StackTraceKey.key, fiber3.currentStackFrame]])
      });
      annotated = true;
    }
    let cont = fiber3.getCont(contE);
    while (fiber3.interruptible && fiber3._interruptedCause && cont) {
      cont = fiber3.getCont(contE);
    }
    return cont ? cont[contE](cause, fiber3, annotated ? void 0 : this) : fiber3.yieldWith(annotated ? this : exitFailCause(cause));
  }
});
var exitFail = (e) => exitFailCause(causeFail(e));
var exitDie = (defect) => exitFailCause(causeDie(defect));
var withFiber = /* @__PURE__ */ makePrimitive({
  op: "WithFiber",
  [evaluate](fiber3) {
    return this[args](fiber3);
  }
});
var YieldableError = /* @__PURE__ */ (function() {
  class YieldableError2 extends globalThis.Error {
    asEffect() {
      return exitFail(this);
    }
  }
  Object.assign(YieldableError2.prototype, YieldableErrorProto);
  return YieldableError2;
})();
var Error2 = /* @__PURE__ */ (function() {
  const plainArgsSymbol = /* @__PURE__ */ Symbol.for("effect/Data/Error/plainArgs");
  return class Base extends YieldableError {
    constructor(args3) {
      super(args3?.message, args3?.cause ? {
        cause: args3.cause
      } : void 0);
      if (args3) {
        Object.assign(this, args3);
        Object.defineProperty(this, plainArgsSymbol, {
          value: args3,
          enumerable: false
        });
      }
    }
    toJSON() {
      return {
        ...this[plainArgsSymbol],
        ...this
      };
    }
  };
})();
var TaggedError = (tag) => {
  class Base extends Error2 {
    _tag = tag;
  }
  ;
  Base.prototype.name = tag;
  return Base;
};
var NoSuchElementErrorTypeId = "~effect/Cause/NoSuchElementError";
var isNoSuchElementError = (u) => hasProperty(u, NoSuchElementErrorTypeId);
var NoSuchElementError = class extends (/* @__PURE__ */ TaggedError("NoSuchElementError")) {
  [NoSuchElementErrorTypeId] = NoSuchElementErrorTypeId;
  constructor(message) {
    super({
      message
    });
  }
};
var DoneTypeId = "~effect/Cause/Done";
var isDone = (u) => hasProperty(u, DoneTypeId);
var DoneVoid = {
  [DoneTypeId]: DoneTypeId,
  _tag: "Done",
  value: void 0
};
var Done = (value) => {
  if (value === void 0) return DoneVoid;
  return {
    [DoneTypeId]: DoneTypeId,
    _tag: "Done",
    value
  };
};
var doneVoid = /* @__PURE__ */ exitFail(DoneVoid);
var done = (value) => {
  if (value === void 0) return doneVoid;
  return exitFail(Done(value));
};

// ../../node_modules/effect/dist/internal/option.js
var TypeId = "~effect/data/Option";
var CommonProto = {
  [TypeId]: {
    _A: (_) => _
  },
  ...PipeInspectableProto,
  ...YieldableProto
};
var SomeProto = /* @__PURE__ */ Object.assign(/* @__PURE__ */ Object.create(CommonProto), {
  _tag: "Some",
  _op: "Some",
  [symbol2](that) {
    return isOption(that) && isSome(that) && equals(this.value, that.value);
  },
  [symbol]() {
    return combine(hash(this._tag))(hash(this.value));
  },
  toString() {
    return `some(${format(this.value)})`;
  },
  toJSON() {
    return {
      _id: "Option",
      _tag: this._tag,
      value: toJson(this.value)
    };
  },
  asEffect() {
    return exitSucceed(this.value);
  }
});
var NoneHash = /* @__PURE__ */ hash("None");
var NoneProto = /* @__PURE__ */ Object.assign(/* @__PURE__ */ Object.create(CommonProto), {
  _tag: "None",
  _op: "None",
  [symbol2](that) {
    return isOption(that) && isNone(that);
  },
  [symbol]() {
    return NoneHash;
  },
  toString() {
    return `none()`;
  },
  toJSON() {
    return {
      _id: "Option",
      _tag: this._tag
    };
  },
  asEffect() {
    return exitFail(new NoSuchElementError());
  }
});
var isOption = (input) => hasProperty(input, TypeId);
var isNone = (fa) => fa._tag === "None";
var isSome = (fa) => fa._tag === "Some";
var none = /* @__PURE__ */ Object.create(NoneProto);
var some = (value) => {
  const a = Object.create(SomeProto);
  a.value = value;
  return a;
};

// ../../node_modules/effect/dist/internal/result.js
var TypeId2 = "~effect/data/Result";
var CommonProto2 = {
  [TypeId2]: {
    /* v8 ignore next 2 */
    _A: (_) => _,
    _E: (_) => _
  },
  ...PipeInspectableProto,
  ...YieldableProto
};
var SuccessProto = /* @__PURE__ */ Object.assign(/* @__PURE__ */ Object.create(CommonProto2), {
  _tag: "Success",
  _op: "Success",
  [symbol2](that) {
    return isResult(that) && isSuccess(that) && equals(this.success, that.success);
  },
  [symbol]() {
    return combine(hash(this._tag))(hash(this.success));
  },
  toString() {
    return `success(${format(this.success)})`;
  },
  toJSON() {
    return {
      _id: "Result",
      _tag: this._tag,
      value: toJson(this.success)
    };
  },
  asEffect() {
    return exitSucceed(this.success);
  }
});
var FailureProto = /* @__PURE__ */ Object.assign(/* @__PURE__ */ Object.create(CommonProto2), {
  _tag: "Failure",
  _op: "Failure",
  [symbol2](that) {
    return isResult(that) && isFailure(that) && equals(this.failure, that.failure);
  },
  [symbol]() {
    return combine(hash(this._tag))(hash(this.failure));
  },
  toString() {
    return `failure(${format(this.failure)})`;
  },
  toJSON() {
    return {
      _id: "Result",
      _tag: this._tag,
      failure: toJson(this.failure)
    };
  },
  asEffect() {
    return exitFail(this.failure);
  }
});
var isResult = (input) => hasProperty(input, TypeId2);
var isFailure = (result3) => result3._tag === "Failure";
var isSuccess = (result3) => result3._tag === "Success";
var fail = (failure) => {
  const a = Object.create(FailureProto);
  a.failure = failure;
  return a;
};
var succeed = (success) => {
  const a = Object.create(SuccessProto);
  a.success = success;
  return a;
};

// ../../node_modules/effect/dist/Order.js
function make(compare) {
  return (self, that) => self === that ? 0 : compare(self, that);
}
var Number2 = /* @__PURE__ */ make((self, that) => {
  if (globalThis.Number.isNaN(self) && globalThis.Number.isNaN(that)) return 0;
  if (globalThis.Number.isNaN(self)) return -1;
  if (globalThis.Number.isNaN(that)) return 1;
  return self < that ? -1 : 1;
});
var mapInput = /* @__PURE__ */ dual(2, (self, f) => make((b1, b2) => self(f(b1), f(b2))));
var isGreaterThan = (O) => dual(2, (self, that) => O(self, that) === 1);

// ../../node_modules/effect/dist/Option.js
var none2 = () => none;
var some2 = some;

// ../../node_modules/effect/dist/Result.js
var succeed2 = succeed;
var fail2 = fail;
var isFailure2 = isFailure;

// ../../node_modules/effect/dist/Filter.js
var apply = (filter4, input, ...args3) => {
  const result3 = filter4(input, ...args3);
  if (result3 === true) return succeed2(input);
  if (result3 === false) return fail2(input);
  return result3;
};
var composePassthrough = /* @__PURE__ */ dual(2, (left, right) => (input) => {
  const leftOut = left(input);
  if (isFailure2(leftOut)) return fail2(input);
  const rightOut = right(leftOut.success);
  if (isFailure2(rightOut)) return fail2(input);
  return rightOut;
});

// ../../node_modules/effect/dist/internal/array.js
var isArrayNonEmpty = (self) => self.length > 0;

// ../../node_modules/effect/dist/Iterable.js
var filter = /* @__PURE__ */ dual(2, (self, predicate) => ({
  [Symbol.iterator]() {
    const iterator = self[Symbol.iterator]();
    let i = 0;
    return {
      next() {
        let result3 = iterator.next();
        while (!result3.done) {
          if (predicate(result3.value, i++)) {
            return {
              done: false,
              value: result3.value
            };
          }
          result3 = iterator.next();
        }
        return {
          done: true,
          value: void 0
        };
      }
    };
  }
}));

// ../../node_modules/effect/dist/Array.js
var Array2 = globalThis.Array;
var fromIterable = (collection) => Array2.isArray(collection) ? collection : Array2.from(collection);
var ensure = (self) => Array2.isArray(self) ? self : [self];
var appendAll = /* @__PURE__ */ dual(2, (self, that) => fromIterable(self).concat(fromIterable(that)));
var isArray = Array2.isArray;
var isReadonlyArrayNonEmpty = isArrayNonEmpty;
function isOutOfBounds(i, as3) {
  return i < 0 || i >= as3.length;
}
var getUnsafe = /* @__PURE__ */ dual(2, (self, index) => {
  const i = Math.floor(index);
  if (isOutOfBounds(i, self)) {
    throw new Error(`Index out of bounds: ${i}`);
  }
  return self[i];
});
var headNonEmpty = /* @__PURE__ */ getUnsafe(0);
var tailNonEmpty = (self) => self.slice(1);
var unionWith = /* @__PURE__ */ dual(3, (self, that, isEquivalent) => {
  const a = fromIterable(self);
  const b = fromIterable(that);
  if (isReadonlyArrayNonEmpty(a)) {
    if (isReadonlyArrayNonEmpty(b)) {
      const dedupe = dedupeWith(isEquivalent);
      return dedupe(appendAll(a, b));
    }
    return a;
  }
  return b;
});
var union = /* @__PURE__ */ dual(2, (self, that) => unionWith(self, that, asEquivalence()));
var partitionMap = /* @__PURE__ */ dual(2, (self, f) => {
  const failures = [];
  const successes = [];
  const as3 = fromIterable(self);
  for (let i = 0; i < as3.length; i++) {
    const e = f(as3[i], i);
    if (isFailure2(e)) {
      failures.push(e.failure);
    } else {
      successes.push(e.success);
    }
  }
  return [failures, successes];
});
var dedupeWith = /* @__PURE__ */ dual(2, (self, isEquivalent) => {
  const input = fromIterable(self);
  if (isReadonlyArrayNonEmpty(input)) {
    const out = [headNonEmpty(input)];
    const rest = tailNonEmpty(input);
    for (const r of rest) {
      if (out.every((a) => !isEquivalent(r, a))) {
        out.push(r);
      }
    }
    return out;
  }
  return [];
});

// ../../node_modules/effect/dist/Duration.js
var TypeId3 = "~effect/time/Duration";
var bigint0 = /* @__PURE__ */ BigInt(0);
var bigint1e3 = /* @__PURE__ */ BigInt(1e3);
var DURATION_REGEXP = /^(-?\d+(?:\.\d+)?)\s+(nanos?|micros?|millis?|seconds?|minutes?|hours?|days?|weeks?)$/;
var fromInputUnsafe = (input) => {
  if (isDuration(input)) return input;
  if (isNumber(input)) return millis(input);
  if (isBigInt(input)) return nanos(input);
  if (Array.isArray(input) && input.length === 2 && input.every(isNumber)) {
    if (Number.isNaN(input[0]) || Number.isNaN(input[1])) {
      return zero;
    }
    if (input[0] === -Infinity || input[1] === -Infinity) {
      return negativeInfinity;
    }
    if (input[0] === Infinity || input[1] === Infinity) {
      return infinity;
    }
    return nanos(BigInt(Math.round(input[0] * 1e9)) + BigInt(Math.round(input[1])));
  }
  if (isString(input)) {
    const match4 = DURATION_REGEXP.exec(input);
    if (match4) {
      const [_, valueStr, unit] = match4;
      const value = Number(valueStr);
      switch (unit) {
        case "nano":
        case "nanos":
          return nanos(BigInt(valueStr));
        case "micro":
        case "micros":
          return micros(BigInt(valueStr));
        case "milli":
        case "millis":
          return millis(value);
        case "second":
        case "seconds":
          return seconds(value);
        case "minute":
        case "minutes":
          return minutes(value);
        case "hour":
        case "hours":
          return hours(value);
        case "day":
        case "days":
          return days(value);
        case "week":
        case "weeks":
          return weeks(value);
      }
    }
  }
  throw new Error(`Invalid Input: ${input}`);
};
var zeroDurationValue = {
  _tag: "Millis",
  millis: 0
};
var infinityDurationValue = {
  _tag: "Infinity"
};
var negativeInfinityDurationValue = {
  _tag: "NegativeInfinity"
};
var DurationProto = {
  [TypeId3]: TypeId3,
  [symbol]() {
    return structure(this.value);
  },
  [symbol2](that) {
    return isDuration(that) && equals2(this, that);
  },
  toString() {
    switch (this.value._tag) {
      case "Infinity":
        return "Infinity";
      case "NegativeInfinity":
        return "-Infinity";
      case "Nanos":
        return `${this.value.nanos} nanos`;
      case "Millis":
        return `${this.value.millis} millis`;
    }
  },
  toJSON() {
    switch (this.value._tag) {
      case "Millis":
        return {
          _id: "Duration",
          _tag: "Millis",
          millis: this.value.millis
        };
      case "Nanos":
        return {
          _id: "Duration",
          _tag: "Nanos",
          nanos: String(this.value.nanos)
        };
      case "Infinity":
        return {
          _id: "Duration",
          _tag: "Infinity"
        };
      case "NegativeInfinity":
        return {
          _id: "Duration",
          _tag: "NegativeInfinity"
        };
    }
  },
  [NodeInspectSymbol]() {
    return this.toJSON();
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var make2 = (input) => {
  const duration = Object.create(DurationProto);
  if (isNumber(input)) {
    if (isNaN(input) || input === 0 || Object.is(input, -0)) {
      duration.value = zeroDurationValue;
    } else if (!Number.isFinite(input)) {
      duration.value = input > 0 ? infinityDurationValue : negativeInfinityDurationValue;
    } else if (!Number.isInteger(input)) {
      duration.value = {
        _tag: "Nanos",
        nanos: BigInt(Math.round(input * 1e6))
      };
    } else {
      duration.value = {
        _tag: "Millis",
        millis: input
      };
    }
  } else if (input === bigint0) {
    duration.value = zeroDurationValue;
  } else {
    duration.value = {
      _tag: "Nanos",
      nanos: input
    };
  }
  return duration;
};
var isDuration = (u) => hasProperty(u, TypeId3);
var zero = /* @__PURE__ */ make2(0);
var infinity = /* @__PURE__ */ make2(Infinity);
var negativeInfinity = /* @__PURE__ */ make2(-Infinity);
var nanos = (nanos2) => make2(nanos2);
var micros = (micros2) => make2(micros2 * bigint1e3);
var millis = (millis2) => make2(millis2);
var seconds = (seconds2) => make2(seconds2 * 1e3);
var minutes = (minutes2) => make2(minutes2 * 6e4);
var hours = (hours2) => make2(hours2 * 36e5);
var days = (days2) => make2(days2 * 864e5);
var weeks = (weeks2) => make2(weeks2 * 6048e5);
var toMillis = (self) => match(fromInputUnsafe(self), {
  onMillis: identity,
  onNanos: (nanos2) => Number(nanos2) / 1e6,
  onInfinity: () => Infinity,
  onNegativeInfinity: () => -Infinity
});
var toNanosUnsafe = (self) => {
  switch (self.value._tag) {
    case "Infinity":
    case "NegativeInfinity":
      throw new Error("Cannot convert infinite duration to nanos");
    case "Nanos":
      return self.value.nanos;
    case "Millis":
      return BigInt(Math.round(self.value.millis * 1e6));
  }
};
var match = /* @__PURE__ */ dual(2, (self, options) => {
  switch (self.value._tag) {
    case "Millis":
      return options.onMillis(self.value.millis);
    case "Nanos":
      return options.onNanos(self.value.nanos);
    case "Infinity":
      return options.onInfinity();
    case "NegativeInfinity":
      return (options.onNegativeInfinity ?? options.onInfinity)();
  }
});
var matchPair = /* @__PURE__ */ dual(3, (self, that, options) => {
  if (self.value._tag === "Infinity" || self.value._tag === "NegativeInfinity" || that.value._tag === "Infinity" || that.value._tag === "NegativeInfinity") return options.onInfinity(self, that);
  if (self.value._tag === "Millis") {
    return that.value._tag === "Millis" ? options.onMillis(self.value.millis, that.value.millis) : options.onNanos(toNanosUnsafe(self), that.value.nanos);
  } else {
    return options.onNanos(self.value.nanos, toNanosUnsafe(that));
  }
});
var Equivalence = (self, that) => matchPair(self, that, {
  onMillis: (self2, that2) => self2 === that2,
  onNanos: (self2, that2) => self2 === that2,
  onInfinity: (self2, that2) => self2.value._tag === that2.value._tag
});
var subtract = /* @__PURE__ */ dual(2, (self, that) => matchPair(self, that, {
  onMillis: (self2, that2) => make2(self2 - that2),
  onNanos: (self2, that2) => make2(self2 - that2),
  onInfinity: (self2, that2) => {
    const s = self2.value._tag;
    const t = that2.value._tag;
    if (s === "Infinity") return t === "Infinity" ? zero : infinity;
    if (s === "NegativeInfinity") return t === "NegativeInfinity" ? zero : negativeInfinity;
    return t === "Infinity" ? negativeInfinity : infinity;
  }
}));
var equals2 = /* @__PURE__ */ dual(2, (self, that) => Equivalence(self, that));

// ../../node_modules/effect/dist/ServiceMap.js
var ServiceMap_exports = {};
__export(ServiceMap_exports, {
  Reference: () => Reference,
  Service: () => Service,
  add: () => add,
  addOrOmit: () => addOrOmit,
  empty: () => empty,
  get: () => get,
  getOption: () => getOption,
  getOrElse: () => getOrElse,
  getOrUndefined: () => getOrUndefined,
  getReferenceUnsafe: () => getReferenceUnsafe,
  getUnsafe: () => getUnsafe2,
  isReference: () => isReference,
  isService: () => isService,
  isServiceMap: () => isServiceMap,
  make: () => make3,
  makeUnsafe: () => makeUnsafe,
  merge: () => merge,
  mergeAll: () => mergeAll,
  omit: () => omit,
  pick: () => pick
});
var ServiceTypeId = "~effect/ServiceMap/Service";
var Service = function() {
  const prevLimit = Error.stackTraceLimit;
  Error.stackTraceLimit = 2;
  const err = new Error();
  Error.stackTraceLimit = prevLimit;
  function KeyClass() {
  }
  const self = KeyClass;
  Object.setPrototypeOf(self, ServiceProto);
  Object.defineProperty(self, "stack", {
    get() {
      return err.stack;
    }
  });
  if (arguments.length > 0) {
    self.key = arguments[0];
    if (arguments[1]?.defaultValue) {
      self[ReferenceTypeId] = ReferenceTypeId;
      self.defaultValue = arguments[1].defaultValue;
    }
    return self;
  }
  return function(key, options) {
    self.key = key;
    if (options?.make) {
      ;
      self.make = options.make;
    }
    return self;
  };
};
var ServiceProto = {
  [ServiceTypeId]: {
    _Service: (_) => _,
    _Identifier: (_) => _
  },
  ...PipeInspectableProto,
  ...YieldableProto,
  toJSON() {
    return {
      _id: "Service",
      key: this.key,
      stack: this.stack
    };
  },
  asEffect() {
    const fn3 = this.asEffect = constant(withFiber((fiber3) => exitSucceed(get(fiber3.services, this))));
    return fn3();
  },
  of(self) {
    return self;
  },
  serviceMap(self) {
    return make3(this, self);
  },
  use(f) {
    return withFiber((fiber3) => f(get(fiber3.services, this)));
  },
  useSync(f) {
    return withFiber((fiber3) => exitSucceed(f(get(fiber3.services, this))));
  }
};
var ReferenceTypeId = "~effect/ServiceMap/Reference";
var TypeId4 = "~effect/ServiceMap";
var makeUnsafe = (mapUnsafe) => {
  const self = Object.create(Proto);
  self.mapUnsafe = mapUnsafe;
  return self;
};
var Proto = {
  ...PipeInspectableProto,
  [TypeId4]: {
    _Services: (_) => _
  },
  toJSON() {
    return {
      _id: "ServiceMap",
      services: Array.from(this.mapUnsafe).map(([key, value]) => ({
        key,
        value
      }))
    };
  },
  [symbol2](that) {
    if (!isServiceMap(that) || this.mapUnsafe.size !== that.mapUnsafe.size) return false;
    for (const k of this.mapUnsafe.keys()) {
      if (!that.mapUnsafe.has(k) || !equals(this.mapUnsafe.get(k), that.mapUnsafe.get(k))) {
        return false;
      }
    }
    return true;
  },
  [symbol]() {
    return number(this.mapUnsafe.size);
  }
};
var isServiceMap = (u) => hasProperty(u, TypeId4);
var isService = (u) => hasProperty(u, ServiceTypeId);
var isReference = (u) => hasProperty(u, ReferenceTypeId);
var empty = () => emptyServiceMap2;
var emptyServiceMap2 = /* @__PURE__ */ makeUnsafe(/* @__PURE__ */ new Map());
var make3 = (key, service3) => makeUnsafe(/* @__PURE__ */ new Map([[key.key, service3]]));
var add = /* @__PURE__ */ dual(3, (self, key, service3) => {
  const map4 = new Map(self.mapUnsafe);
  map4.set(key.key, service3);
  return makeUnsafe(map4);
});
var addOrOmit = /* @__PURE__ */ dual(3, (self, key, service3) => {
  const map4 = new Map(self.mapUnsafe);
  if (service3._tag === "None") {
    map4.delete(key.key);
  } else {
    map4.set(key.key, service3.value);
  }
  return makeUnsafe(map4);
});
var getOrElse = /* @__PURE__ */ dual(3, (self, key, orElse) => {
  if (self.mapUnsafe.has(key.key)) {
    return self.mapUnsafe.get(key.key);
  }
  return isReference(key) ? getDefaultValue(key) : orElse();
});
var getOrUndefined = /* @__PURE__ */ dual(2, (self, key) => self.mapUnsafe.get(key.key));
var getUnsafe2 = /* @__PURE__ */ dual(2, (self, service3) => {
  if (!self.mapUnsafe.has(service3.key)) {
    if (ReferenceTypeId in service3) return getDefaultValue(service3);
    throw serviceNotFoundError(service3);
  }
  return self.mapUnsafe.get(service3.key);
});
var get = getUnsafe2;
var getReferenceUnsafe = (self, service3) => {
  if (!self.mapUnsafe.has(service3.key)) {
    return getDefaultValue(service3);
  }
  return self.mapUnsafe.get(service3.key);
};
var defaultValueCacheKey = "~effect/ServiceMap/defaultValue";
var getDefaultValue = (ref) => {
  if (defaultValueCacheKey in ref) {
    return ref[defaultValueCacheKey];
  }
  return ref[defaultValueCacheKey] = ref.defaultValue();
};
var serviceNotFoundError = (service3) => {
  const error = new Error(`Service not found${service3.key ? `: ${String(service3.key)}` : ""}`);
  if (service3.stack) {
    const lines = service3.stack.split("\n");
    if (lines.length > 2) {
      const afterAt = lines[2].match(/at (.*)/);
      if (afterAt) {
        error.message = error.message + ` (defined at ${afterAt[1]})`;
      }
    }
  }
  if (error.stack) {
    const lines = error.stack.split("\n");
    lines.splice(1, 3);
    error.stack = lines.join("\n");
  }
  return error;
};
var getOption = /* @__PURE__ */ dual(2, (self, service3) => {
  if (self.mapUnsafe.has(service3.key)) {
    return some2(self.mapUnsafe.get(service3.key));
  }
  return isReference(service3) ? some2(getDefaultValue(service3)) : none2();
});
var merge = /* @__PURE__ */ dual(2, (self, that) => {
  if (self.mapUnsafe.size === 0) return that;
  if (that.mapUnsafe.size === 0) return self;
  const map4 = new Map(self.mapUnsafe);
  that.mapUnsafe.forEach((value, key) => map4.set(key, value));
  return makeUnsafe(map4);
});
var mergeAll = (...ctxs) => {
  const map4 = /* @__PURE__ */ new Map();
  for (let i = 0; i < ctxs.length; i++) {
    ctxs[i].mapUnsafe.forEach((value, key) => {
      map4.set(key, value);
    });
  }
  return makeUnsafe(map4);
};
var pick = (...services3) => (self) => {
  const map4 = /* @__PURE__ */ new Map();
  const keySet = new Set(services3.map((key) => key.key));
  self.mapUnsafe.forEach((value, key) => {
    if (keySet.has(key)) {
      map4.set(key, value);
    }
  });
  return makeUnsafe(map4);
};
var omit = (...keys) => (self) => {
  const map4 = new Map(self.mapUnsafe);
  for (let i = 0; i < keys.length; i++) {
    map4.delete(keys[i].key);
  }
  return makeUnsafe(map4);
};
var Reference = Service;

// ../../node_modules/effect/dist/Scheduler.js
var Scheduler = /* @__PURE__ */ Reference("effect/Scheduler", {
  defaultValue: () => new MixedScheduler()
});
var setImmediate = "setImmediate" in globalThis ? (f) => {
  const timer = globalThis.setImmediate(f);
  return () => globalThis.clearImmediate(timer);
} : (f) => {
  const timer = setTimeout(f, 0);
  return () => clearTimeout(timer);
};
var PriorityBuckets = class {
  buckets = [];
  scheduleTask(task, priority) {
    const buckets = this.buckets;
    const len = buckets.length;
    let bucket;
    let index = 0;
    for (; index < len; index++) {
      if (buckets[index][0] > priority) break;
      bucket = buckets[index];
    }
    if (bucket && bucket[0] === priority) {
      bucket[1].push(task);
    } else if (index === len) {
      buckets.push([priority, [task]]);
    } else {
      buckets.splice(index, 0, [priority, [task]]);
    }
  }
  drain() {
    const buckets = this.buckets;
    this.buckets = [];
    return buckets;
  }
};
var MixedScheduler = class {
  tasks = /* @__PURE__ */ new PriorityBuckets();
  running = void 0;
  executionMode;
  setImmediate;
  constructor(executionMode = "async", setImmediateFn = setImmediate) {
    this.executionMode = executionMode;
    this.setImmediate = setImmediateFn;
  }
  /**
   * @since 2.0.0
   */
  scheduleTask(task, priority) {
    this.tasks.scheduleTask(task, priority);
    if (this.running === void 0) {
      this.running = this.setImmediate(this.afterScheduled);
    }
  }
  /**
   * @since 2.0.0
   */
  afterScheduled = () => {
    this.running = void 0;
    this.runTasks();
  };
  /**
   * @since 2.0.0
   */
  runTasks() {
    const buckets = this.tasks.drain();
    for (let i = 0; i < buckets.length; i++) {
      const toRun = buckets[i][1];
      for (let j = 0; j < toRun.length; j++) {
        toRun[j]();
      }
    }
  }
  /**
   * @since 2.0.0
   */
  shouldYield(fiber3) {
    return fiber3.currentOpCount >= fiber3.maxOpsBeforeYield;
  }
  /**
   * @since 2.0.0
   */
  flush() {
    while (this.tasks.buckets.length > 0) {
      if (this.running !== void 0) {
        this.running();
        this.running = void 0;
      }
      this.runTasks();
    }
  }
};
var MaxOpsBeforeYield = /* @__PURE__ */ Reference("effect/Scheduler/MaxOpsBeforeYield", {
  defaultValue: () => 2048
});

// ../../node_modules/effect/dist/Tracer.js
var ParentSpanKey = "effect/Tracer/ParentSpan";
var ParentSpan = class extends (/* @__PURE__ */ Service()(ParentSpanKey)) {
};
var make4 = (options) => options;
var DisablePropagation = /* @__PURE__ */ Reference("effect/Tracer/DisablePropagation", {
  defaultValue: constFalse
});
var CurrentTraceLevel = /* @__PURE__ */ Reference("effect/Tracer/CurrentTraceLevel", {
  defaultValue: () => "Info"
});
var MinimumTraceLevel = /* @__PURE__ */ Reference("effect/Tracer/MinimumTraceLevel", {
  defaultValue: () => "All"
});
var TracerKey = "effect/Tracer";
var Tracer = /* @__PURE__ */ Reference(TracerKey, {
  defaultValue: () => make4({
    span: (options) => new NativeSpan(options)
  })
});
var NativeSpan = class {
  _tag = "Span";
  spanId;
  traceId = "native";
  sampled;
  name;
  parent;
  annotations;
  links;
  startTime;
  kind;
  status;
  attributes;
  events = [];
  constructor(options) {
    this.name = options.name;
    this.parent = options.parent;
    this.annotations = options.annotations;
    this.links = options.links;
    this.startTime = options.startTime;
    this.kind = options.kind;
    this.sampled = options.sampled;
    this.status = {
      _tag: "Started",
      startTime: options.startTime
    };
    this.attributes = /* @__PURE__ */ new Map();
    this.traceId = options.parent?.traceId ?? randomHexString(32);
    this.spanId = randomHexString(16);
  }
  end(endTime, exit3) {
    this.status = {
      _tag: "Ended",
      endTime,
      exit: exit3,
      startTime: this.status.startTime
    };
  }
  attribute(key, value) {
    this.attributes.set(key, value);
  }
  event(name, startTime, attributes) {
    this.events.push([name, startTime, attributes ?? {}]);
  }
  addLinks(links) {
    this.links.push(...links);
  }
};
var randomHexString = /* @__PURE__ */ (function() {
  const characters = "abcdef0123456789";
  const charactersLength = characters.length;
  return function(length) {
    let result3 = "";
    for (let i = 0; i < length; i++) {
      result3 += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result3;
  };
})();

// ../../node_modules/effect/dist/References.js
var CurrentConcurrency = /* @__PURE__ */ Reference("effect/References/CurrentConcurrency", {
  defaultValue: () => "unbounded"
});
var CurrentStackFrame = /* @__PURE__ */ Reference("effect/References/CurrentStackFrame", {
  defaultValue: constUndefined
});
var TracerEnabled = /* @__PURE__ */ Reference("effect/References/TracerEnabled", {
  defaultValue: constTrue
});
var TracerTimingEnabled = /* @__PURE__ */ Reference("effect/References/TracerTimingEnabled", {
  defaultValue: constTrue
});
var TracerSpanAnnotations = /* @__PURE__ */ Reference("effect/References/TracerSpanAnnotations", {
  defaultValue: () => ({})
});
var TracerSpanLinks = /* @__PURE__ */ Reference("effect/References/TracerSpanLinks", {
  defaultValue: () => []
});
var CurrentLogAnnotations = /* @__PURE__ */ Reference("effect/References/CurrentLogAnnotations", {
  defaultValue: () => ({})
});
var CurrentLogLevel = /* @__PURE__ */ Reference("effect/References/CurrentLogLevel", {
  defaultValue: () => "Info"
});
var MinimumLogLevel = /* @__PURE__ */ Reference("effect/References/MinimumLogLevel", {
  defaultValue: () => "Info"
});
var CurrentLogSpans = /* @__PURE__ */ Reference("effect/References/CurrentLogSpans", {
  defaultValue: () => []
});

// ../../node_modules/effect/dist/internal/metric.js
var FiberRuntimeMetricsKey = "effect/observability/Metric/FiberRuntimeMetricsKey";

// ../../node_modules/effect/dist/internal/tracer.js
var addSpanStackTrace = (options) => {
  if (options?.captureStackTrace === false) {
    return options;
  } else if (options?.captureStackTrace !== void 0 && typeof options.captureStackTrace !== "boolean") {
    return options;
  }
  const limit = Error.stackTraceLimit;
  Error.stackTraceLimit = 3;
  const traceError = new Error();
  Error.stackTraceLimit = limit;
  return {
    ...options,
    captureStackTrace: spanCleaner(() => traceError.stack)
  };
};
var makeStackCleaner = (line) => (stack) => {
  let cache;
  return () => {
    if (cache !== void 0) return cache;
    const trace = stack();
    if (!trace) return void 0;
    const lines = trace.split("\n");
    if (lines[line] !== void 0) {
      cache = lines[line].trim();
      return cache;
    }
  };
};
var spanCleaner = /* @__PURE__ */ makeStackCleaner(3);

// ../../node_modules/effect/dist/internal/version.js
var version = "dev";

// ../../node_modules/effect/dist/internal/effect.js
var Interrupt = class extends ReasonBase {
  fiberId;
  constructor(fiberId3, annotations = constEmptyAnnotations) {
    super("Interrupt", annotations, "Interrupted");
    this.fiberId = fiberId3;
  }
  toString() {
    return `Interrupt(${this.fiberId})`;
  }
  toJSON() {
    return {
      _tag: "Interrupt",
      fiberId: this.fiberId
    };
  }
  [symbol2](that) {
    return isInterruptReason(that) && this.fiberId === that.fiberId && this.annotations === that.annotations;
  }
  [symbol]() {
    return combine(string(`${this._tag}:${this.fiberId}`))(random(this.annotations));
  }
};
var causeInterrupt = (fiberId3) => new CauseImpl([new Interrupt(fiberId3)]);
var findFail = (self) => {
  const reason = self.reasons.find(isFailReason);
  return reason ? succeed2(reason) : fail2(self);
};
var findError = (self) => {
  for (let i = 0; i < self.reasons.length; i++) {
    const reason = self.reasons[i];
    if (reason._tag === "Fail") {
      return succeed2(reason.error);
    }
  }
  return fail2(self);
};
var findDefect = (self) => {
  const reason = self.reasons.find(isDieReason);
  return reason ? succeed2(reason.defect) : fail2(self);
};
var hasInterrupts = (self) => self.reasons.some(isInterruptReason);
var causeFilterInterruptors = (self) => {
  let interruptors;
  for (let i = 0; i < self.reasons.length; i++) {
    const f = self.reasons[i];
    if (f._tag !== "Interrupt") continue;
    interruptors ??= /* @__PURE__ */ new Set();
    if (f.fiberId !== void 0) {
      interruptors.add(f.fiberId);
    }
  }
  return interruptors ? succeed2(interruptors) : fail2(self);
};
var causeCombine = /* @__PURE__ */ dual(2, (self, that) => {
  if (self.reasons.length === 0) {
    return that;
  } else if (that.reasons.length === 0) {
    return self;
  }
  const newCause = new CauseImpl(union(self.reasons, that.reasons));
  return equals(self, newCause) ? self : newCause;
});
var causePartition = (self) => {
  const obj = {
    Fail: [],
    Die: [],
    Interrupt: []
  };
  for (let i = 0; i < self.reasons.length; i++) {
    obj[self.reasons[i]._tag].push(self.reasons[i]);
  }
  return obj;
};
var causeSquash = (self) => {
  const partitioned = causePartition(self);
  if (partitioned.Fail.length > 0) {
    return partitioned.Fail[0].error;
  } else if (partitioned.Die.length > 0) {
    return partitioned.Die[0].defect;
  } else if (partitioned.Interrupt.length > 0) {
    return new globalThis.Error("All fibers interrupted without error");
  }
  return new globalThis.Error("Empty cause");
};
var causePrettyErrors = (self) => {
  const errors = [];
  const interrupts = [];
  if (self.reasons.length === 0) return errors;
  const prevStackLimit = Error.stackTraceLimit;
  Error.stackTraceLimit = 1;
  for (const failure of self.reasons) {
    if (failure._tag === "Interrupt") {
      interrupts.push(failure);
      continue;
    }
    errors.push(causePrettyError(failure._tag === "Die" ? failure.defect : failure.error, failure.annotations));
  }
  if (errors.length === 0) {
    const cause = new Error("The fiber was interrupted by:");
    cause.name = "InterruptCause";
    cause.stack = interruptCauseStack(cause, interrupts);
    const error = new globalThis.Error("All fibers interrupted without error", {
      cause
    });
    error.name = "InterruptError";
    error.stack = `${error.name}: ${error.message}`;
    errors.push(causePrettyError(error, interrupts[0].annotations));
  }
  ;
  Error.stackTraceLimit = prevStackLimit;
  return errors;
};
var causePrettyError = (original, annotations) => {
  const kind = typeof original;
  let error;
  if (original && kind === "object") {
    error = new globalThis.Error(causePrettyMessage(original), {
      cause: original.cause ? causePrettyError(original.cause) : void 0
    });
    if (typeof original.name === "string") {
      error.name = original.name;
    }
    if (typeof original.stack === "string") {
      error.stack = cleanErrorStack(original.stack, error, annotations);
    } else {
      const stack = `${error.name}: ${error.message}`;
      error.stack = annotations ? addStackAnnotations(stack, annotations) : stack;
    }
    for (const key of Object.keys(original)) {
      if (!(key in error)) {
        ;
        error[key] = original[key];
      }
    }
  } else {
    error = new globalThis.Error(!original ? `Unknown error: ${original}` : kind === "string" ? original : formatJson(original));
  }
  return error;
};
var causePrettyMessage = (u) => {
  if (typeof u.message === "string") {
    return u.message;
  } else if (typeof u.toString === "function" && u.toString !== Object.prototype.toString && u.toString !== Array.prototype.toString) {
    try {
      return u.toString();
    } catch {
    }
  }
  return formatJson(u);
};
var locationRegExp = /\((.*)\)/g;
var cleanErrorStack = (stack, error, annotations) => {
  const message = `${error.name}: ${error.message}`;
  const lines = (stack.startsWith(message) ? stack.slice(message.length) : stack).split("\n");
  const out = [message];
  for (let i = 1; i < lines.length; i++) {
    if (/(?:Generator\.next|~effect\/Effect)/.test(lines[i])) {
      break;
    }
    out.push(lines[i]);
  }
  return annotations ? addStackAnnotations(out.join("\n"), annotations) : out.join("\n");
};
var addStackAnnotations = (stack, annotations) => {
  const frame = annotations?.get(StackTraceKey.key);
  if (frame) {
    stack = `${stack}
${currentStackTrace(frame)}`;
  }
  return stack;
};
var interruptCauseStack = (error, interrupts) => {
  const out = [`${error.name}: ${error.message}`];
  for (const current of interrupts) {
    const fiberId3 = current.fiberId !== void 0 ? `#${current.fiberId}` : "unknown";
    const frame = current.annotations.get(InterruptorStackTrace.key);
    out.push(`    at fiber (${fiberId3})`);
    if (frame) out.push(currentStackTrace(frame));
  }
  return out.join("\n");
};
var currentStackTrace = (frame) => {
  const out = [];
  let current = frame;
  let i = 0;
  while (current && i < 10) {
    const stack = current.stack();
    if (stack) {
      const locationMatchAll = stack.matchAll(locationRegExp);
      let match4 = false;
      for (const [, location] of locationMatchAll) {
        match4 = true;
        out.push(`    at ${current.name} (${location})`);
      }
      if (!match4) {
        out.push(`    at ${current.name} (${stack.replace(/^at /, "")})`);
      }
    } else {
      out.push(`    at ${current.name}`);
    }
    current = current.parent;
    i++;
  }
  return out.join("\n");
};
var causePretty = (cause) => causePrettyErrors(cause).map((e) => e.cause ? `${e.stack} {
${renderErrorCause(e.cause, "  ")}
}` : e.stack).join("\n");
var renderErrorCause = (cause, prefix) => {
  const lines = cause.stack.split("\n");
  let stack = `${prefix}[cause]: ${lines[0]}`;
  for (let i = 1, len = lines.length; i < len; i++) {
    stack += `
${prefix}${lines[i]}`;
  }
  if (cause.cause) {
    stack += ` {
${renderErrorCause(cause.cause, `${prefix}  `)}
${prefix}}`;
  }
  return stack;
};
var FiberTypeId = `~effect/Fiber/${version}`;
var fiberVariance = {
  _A: identity,
  _E: identity
};
var fiberIdStore = {
  id: 0
};
var getCurrentFiber = () => globalThis[currentFiberTypeId];
var keepAlive = /* @__PURE__ */ (() => {
  let count = 0;
  let running = void 0;
  return {
    increment() {
      count++;
      running ??= globalThis.setInterval(constVoid, 2147483647);
    },
    decrement() {
      count--;
      if (count === 0 && running !== void 0) {
        globalThis.clearInterval(running);
        running = void 0;
      }
    }
  };
})();
var FiberImpl = class {
  constructor(services3, interruptible3 = true) {
    this[FiberTypeId] = fiberVariance;
    this.setServices(services3);
    this.id = ++fiberIdStore.id;
    this.currentOpCount = 0;
    this.currentLoopCount = 0;
    this.interruptible = interruptible3;
    this._stack = [];
    this._observers = [];
    this._exit = void 0;
    this._children = void 0;
    this._interruptedCause = void 0;
    this._yielded = void 0;
  }
  [FiberTypeId];
  id;
  interruptible;
  currentOpCount;
  currentLoopCount;
  _stack;
  _observers;
  _exit;
  _currentExit;
  _children;
  _interruptedCause;
  _yielded;
  // set in setServices
  services;
  currentScheduler;
  currentTracerContext;
  currentSpan;
  currentLogLevel;
  minimumLogLevel;
  currentStackFrame;
  runtimeMetrics;
  maxOpsBeforeYield;
  getRef(ref) {
    return getReferenceUnsafe(this.services, ref);
  }
  addObserver(cb) {
    if (this._exit) {
      cb(this._exit);
      return constVoid;
    }
    this._observers.push(cb);
    return () => {
      const index = this._observers.indexOf(cb);
      if (index >= 0) {
        this._observers.splice(index, 1);
      }
    };
  }
  interruptUnsafe(fiberId3, annotations) {
    if (this._exit) {
      return;
    }
    let cause = causeInterrupt(fiberId3);
    if (this.currentStackFrame) {
      cause = causeAnnotate(cause, make3(StackTraceKey, this.currentStackFrame));
    }
    if (annotations) {
      cause = causeAnnotate(cause, annotations);
    }
    this._interruptedCause = this._interruptedCause ? causeCombine(this._interruptedCause, cause) : cause;
    if (this.interruptible) {
      this.evaluate(failCause(this._interruptedCause));
    }
  }
  pollUnsafe() {
    return this._exit;
  }
  evaluate(effect2) {
    this.runtimeMetrics?.recordFiberStart(this.services);
    if (this._exit) {
      return;
    } else if (this._yielded !== void 0) {
      const yielded = this._yielded;
      this._yielded = void 0;
      yielded();
    }
    const exit3 = this.runLoop(effect2);
    if (exit3 === Yield) {
      return;
    }
    const interruptChildren = fiberMiddleware.interruptChildren && fiberMiddleware.interruptChildren(this);
    if (interruptChildren !== void 0) {
      return this.evaluate(flatMap(interruptChildren, () => exit3));
    }
    this._exit = exit3;
    this.runtimeMetrics?.recordFiberEnd(this.services, this._exit);
    for (let i = 0; i < this._observers.length; i++) {
      this._observers[i](exit3);
    }
    this._observers.length = 0;
  }
  runLoop(effect2) {
    const prevFiber = globalThis[currentFiberTypeId];
    globalThis[currentFiberTypeId] = this;
    let yielding = false;
    let current = effect2;
    this.currentOpCount = 0;
    const currentLoop = ++this.currentLoopCount;
    try {
      while (true) {
        this.currentOpCount++;
        if (!yielding && this.currentScheduler.shouldYield(this)) {
          yielding = true;
          const prev = current;
          current = flatMap(yieldNow, () => prev);
        }
        current = this.currentTracerContext ? this.currentTracerContext(current, this) : current[evaluate](this);
        if (currentLoop !== this.currentLoopCount) {
          return Yield;
        } else if (current === Yield) {
          const yielded = this._yielded;
          if (ExitTypeId in yielded) {
            this._yielded = void 0;
            return yielded;
          }
          return Yield;
        }
      }
    } catch (error) {
      if (!hasProperty(current, evaluate)) {
        return exitDie(`Fiber.runLoop: Not a valid effect: ${String(current)}`);
      }
      return this.runLoop(exitDie(error));
    } finally {
      ;
      globalThis[currentFiberTypeId] = prevFiber;
    }
  }
  getCont(symbol3) {
    while (true) {
      const op = this._stack.pop();
      if (!op) return void 0;
      const cont = op[contAll] && op[contAll](this);
      if (cont) {
        ;
        cont[symbol3] = cont;
        return cont;
      }
      if (op[symbol3]) return op;
    }
  }
  yieldWith(value) {
    this._yielded = value;
    return Yield;
  }
  children() {
    return this._children ??= /* @__PURE__ */ new Set();
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
  setServices(services3) {
    this.services = services3;
    this.currentScheduler = this.getRef(Scheduler);
    this.currentSpan = services3.mapUnsafe.get(ParentSpanKey);
    this.currentLogLevel = this.getRef(CurrentLogLevel);
    this.minimumLogLevel = this.getRef(MinimumLogLevel);
    this.currentStackFrame = services3.mapUnsafe.get(CurrentStackFrame.key);
    this.maxOpsBeforeYield = this.getRef(MaxOpsBeforeYield);
    this.runtimeMetrics = services3.mapUnsafe.get(FiberRuntimeMetricsKey);
    const currentTracer = services3.mapUnsafe.get(TracerKey);
    this.currentTracerContext = currentTracer ? currentTracer["context"] : void 0;
  }
  get currentSpanLocal() {
    return this.currentSpan?._tag === "Span" ? this.currentSpan : void 0;
  }
};
var fiberMiddleware = {
  interruptChildren: void 0
};
var fiberStackAnnotations = (fiber3) => {
  if (!fiber3.currentStackFrame) return void 0;
  const annotations = /* @__PURE__ */ new Map();
  annotations.set(StackTraceKey.key, fiber3.currentStackFrame);
  return makeUnsafe(annotations);
};
var fiberInterruptChildren = (fiber3) => {
  if (fiber3._children === void 0 || fiber3._children.size === 0) {
    return void 0;
  }
  return fiberInterruptAll(fiber3._children);
};
var fiberAwait = (self) => {
  const impl = self;
  if (impl._exit) return succeed3(impl._exit);
  return callback((resume) => {
    if (impl._exit) return resume(succeed3(impl._exit));
    return sync(self.addObserver((exit3) => resume(succeed3(exit3))));
  });
};
var fiberAwaitAll = (self) => callback((resume) => {
  const iter = self[Symbol.iterator]();
  const exits = [];
  let cancel = void 0;
  function loop() {
    let result3 = iter.next();
    while (!result3.done) {
      if (result3.value._exit) {
        exits.push(result3.value._exit);
        result3 = iter.next();
        continue;
      }
      cancel = result3.value.addObserver((exit3) => {
        exits.push(exit3);
        loop();
      });
      return;
    }
    resume(succeed3(exits));
  }
  loop();
  return sync(() => cancel?.());
});
var fiberInterrupt = (self) => withFiber((fiber3) => fiberInterruptAs(self, fiber3.id));
var fiberInterruptAs = /* @__PURE__ */ dual(2, (self, fiberId3) => withFiber((parent) => {
  self.interruptUnsafe(fiberId3, fiberStackAnnotations(parent));
  return asVoid(fiberAwait(self));
}));
var fiberInterruptAll = (fibers) => withFiber((parent) => {
  const annotations = fiberStackAnnotations(parent);
  for (const fiber3 of fibers) {
    fiber3.interruptUnsafe(parent.id, annotations);
  }
  return asVoid(fiberAwaitAll(fibers));
});
var succeed3 = exitSucceed;
var failCause = exitFailCause;
var fail3 = exitFail;
var sync = /* @__PURE__ */ makePrimitive({
  op: "Sync",
  [evaluate](fiber3) {
    const value = this[args]();
    const cont = fiber3.getCont(contA);
    return cont ? cont[contA](value, fiber3) : fiber3.yieldWith(exitSucceed(value));
  }
});
var suspend = /* @__PURE__ */ makePrimitive({
  op: "Suspend",
  [evaluate](_fiber) {
    return this[args]();
  }
});
var fromYieldable = (yieldable) => yieldable.asEffect();
var fromOption2 = fromYieldable;
var fromResult = fromYieldable;
var fromNullishOr = (value) => value == null ? fail3(new NoSuchElementError()) : succeed3(value);
var yieldNowWith = /* @__PURE__ */ makePrimitive({
  op: "Yield",
  [evaluate](fiber3) {
    let resumed = false;
    fiber3.currentScheduler.scheduleTask(() => {
      if (resumed) return;
      fiber3.evaluate(exitVoid);
    }, this[args] ?? 0);
    return fiber3.yieldWith(() => {
      resumed = true;
    });
  }
});
var yieldNow = /* @__PURE__ */ yieldNowWith(0);
var succeedSome = (a) => succeed3(some2(a));
var succeedNone = /* @__PURE__ */ succeed3(/* @__PURE__ */ none2());
var failCauseSync = (evaluate2) => suspend(() => failCause(internalCall(evaluate2)));
var die = (defect) => exitDie(defect);
var failSync = (error) => suspend(() => fail3(internalCall(error)));
var void_ = /* @__PURE__ */ succeed3(void 0);
var try_ = (options) => suspend(() => {
  try {
    return succeed3(internalCall(options.try));
  } catch (err) {
    return fail3(internalCall(() => options.catch(err)));
  }
});
var promise = (evaluate2) => callbackOptions(function(resume, signal) {
  internalCall(() => evaluate2(signal)).then((a) => resume(succeed3(a)), (e) => resume(die(e)));
}, evaluate2.length !== 0);
var tryPromise = (options) => {
  const f = typeof options === "function" ? options : options.try;
  const catcher = typeof options === "function" ? (cause) => new UnknownError(cause, "An error occurred in Effect.tryPromise") : options.catch;
  return callbackOptions(function(resume, signal) {
    try {
      internalCall(() => f(signal)).then((a) => resume(succeed3(a)), (e) => resume(fail3(internalCall(() => catcher(e)))));
    } catch (err) {
      resume(fail3(internalCall(() => catcher(err))));
    }
  }, eval.length !== 0);
};
var withFiberId = (f) => withFiber((fiber3) => f(fiber3.id));
var fiber = /* @__PURE__ */ withFiber(succeed3);
var fiberId = /* @__PURE__ */ withFiberId(succeed3);
var callbackOptions = /* @__PURE__ */ makePrimitive({
  op: "Async",
  single: false,
  [evaluate](fiber3) {
    const register = internalCall(() => this[args][0].bind(fiber3.currentScheduler));
    let resumed = false;
    let yielded = false;
    const controller = this[args][1] ? new AbortController() : void 0;
    const onCancel = register((effect2) => {
      if (resumed) return;
      resumed = true;
      if (yielded) {
        fiber3.evaluate(effect2);
      } else {
        yielded = effect2;
      }
    }, controller?.signal);
    if (yielded !== false) return yielded;
    yielded = true;
    keepAlive.increment();
    fiber3._yielded = () => {
      resumed = true;
      keepAlive.decrement();
    };
    if (controller === void 0 && onCancel === void 0) {
      return Yield;
    }
    fiber3._stack.push(asyncFinalizer(() => {
      resumed = true;
      controller?.abort();
      return onCancel ?? exitVoid;
    }));
    return Yield;
  }
});
var asyncFinalizer = /* @__PURE__ */ makePrimitive({
  op: "AsyncFinalizer",
  [contAll](fiber3) {
    if (fiber3.interruptible) {
      fiber3.interruptible = false;
      fiber3._stack.push(setInterruptibleTrue);
    }
  },
  [contE](cause, _fiber) {
    return hasInterrupts(cause) ? flatMap(this[args](), () => failCause(cause)) : failCause(cause);
  }
});
var callback = (register) => callbackOptions(register, register.length >= 2);
var never = /* @__PURE__ */ callback(constVoid);
var gen = (...args3) => suspend(() => fromIteratorUnsafe(args3.length === 1 ? args3[0]() : args3[1].call(args3[0].self)));
var fnUntraced = (body, ...pipeables) => {
  return pipeables.length === 0 ? function() {
    return suspend(() => fromIteratorUnsafe(body.apply(this, arguments)));
  } : function() {
    let effect2 = suspend(() => fromIteratorUnsafe(body.apply(this, arguments)));
    for (let i = 0; i < pipeables.length; i++) {
      effect2 = pipeables[i](effect2, ...arguments);
    }
    return effect2;
  };
};
var fnStackCleaner = /* @__PURE__ */ makeStackCleaner(2);
var fn = function() {
  const nameFirst = typeof arguments[0] === "string";
  const name = nameFirst ? arguments[0] : "Effect.fn";
  const spanOptions = nameFirst ? arguments[1] : void 0;
  const prevLimit = globalThis.Error.stackTraceLimit;
  globalThis.Error.stackTraceLimit = 2;
  const defError = new globalThis.Error();
  globalThis.Error.stackTraceLimit = prevLimit;
  if (nameFirst) {
    return (body, ...pipeables) => makeFn(name, body, defError, pipeables, nameFirst, spanOptions);
  }
  return makeFn(name, arguments[0], defError, Array.prototype.slice.call(arguments, 1), nameFirst, spanOptions);
};
var makeFn = (name, bodyOrOptions, defError, pipeables, addSpan, spanOptions) => {
  const body = typeof bodyOrOptions === "function" ? bodyOrOptions : pipeables.pop().bind(bodyOrOptions.self);
  return function(...args3) {
    let result3 = suspend(() => {
      const iter = body.apply(this, arguments);
      return isEffect(iter) ? iter : fromIteratorUnsafe(iter);
    });
    for (let i = 0; i < pipeables.length; i++) {
      result3 = pipeables[i](result3, ...args3);
    }
    if (!isEffect(result3)) {
      return result3;
    }
    const prevLimit = globalThis.Error.stackTraceLimit;
    globalThis.Error.stackTraceLimit = 2;
    const callError = new globalThis.Error();
    globalThis.Error.stackTraceLimit = prevLimit;
    return updateService(addSpan ? useSpan(name, spanOptions, (span2) => provideParentSpan(result3, span2)) : result3, CurrentStackFrame, (prev) => ({
      name,
      stack: fnStackCleaner(() => callError.stack),
      parent: {
        name: `${name} (definition)`,
        stack: fnStackCleaner(() => defError.stack),
        parent: prev
      }
    }));
  };
};
var fnUntracedEager = (body, ...pipeables) => pipeables.length === 0 ? function() {
  return fromIteratorEagerUnsafe(() => body.apply(this, arguments));
} : function() {
  let effect2 = fromIteratorEagerUnsafe(() => body.apply(this, arguments));
  for (const pipeable of pipeables) {
    effect2 = pipeable(effect2);
  }
  return effect2;
};
var fromIteratorEagerUnsafe = (evaluate2) => {
  try {
    const iterator = evaluate2();
    let value = void 0;
    while (true) {
      const state = iterator.next(value);
      if (state.done) {
        return succeed3(state.value);
      }
      const yieldable = state.value;
      const effect2 = yieldable.asEffect();
      const primitive = effect2;
      if (primitive && primitive._tag === "Success") {
        value = primitive.value;
        continue;
      } else if (primitive && primitive._tag === "Failure") {
        return effect2;
      } else {
        let isFirstExecution = true;
        return suspend(() => {
          if (isFirstExecution) {
            isFirstExecution = false;
            return flatMap(effect2, (value2) => fromIteratorUnsafe(iterator, value2));
          } else {
            return suspend(() => fromIteratorUnsafe(evaluate2()));
          }
        });
      }
    }
  } catch (error) {
    return die(error);
  }
};
var fromIteratorUnsafe = /* @__PURE__ */ makePrimitive({
  op: "Iterator",
  single: false,
  [contA](value, fiber3) {
    const iter = this[args][0];
    while (true) {
      const state = iter.next(value);
      if (state.done) return succeed3(state.value);
      const eff = state.value.asEffect();
      if (!effectIsExit(eff)) {
        fiber3._stack.push(this);
        return eff;
      } else if (eff._tag === "Failure") {
        return eff;
      }
      value = eff.value;
    }
  },
  [evaluate](fiber3) {
    return this[contA](this[args][1], fiber3);
  }
});
var as = /* @__PURE__ */ dual(2, (self, value) => {
  const b = succeed3(value);
  return flatMap(self, (_) => b);
});
var asSome = (self) => map(self, some2);
var flip = (self) => matchEffect(self, {
  onFailure: succeed3,
  onSuccess: fail3
});
var andThen = /* @__PURE__ */ dual(2, (self, f) => flatMap(self, (a) => isEffect(f) ? f : internalCall(() => f(a))));
var tap = /* @__PURE__ */ dual(2, (self, f) => flatMap(self, (a) => as(isEffect(f) ? f : internalCall(() => f(a)), a)));
var asVoid = (self) => flatMap(self, (_) => exitVoid);
var sandbox = (self) => catchCause(self, fail3);
var raceAll = (all3, options) => withFiber((parent) => callback((resume) => {
  const effects = fromIterable(all3);
  const len = effects.length;
  let doneCount = 0;
  let done4 = false;
  const fibers = /* @__PURE__ */ new Set();
  const failures = [];
  const onExit3 = (exit3, fiber3, i) => {
    doneCount++;
    if (exit3._tag === "Failure") {
      failures.push(...exit3.cause.reasons);
      if (doneCount >= len) {
        resume(failCause(causeFromReasons(failures)));
      }
      return;
    }
    const isWinner = !done4;
    done4 = true;
    resume(fibers.size === 0 ? exit3 : flatMap(uninterruptible(fiberInterruptAll(fibers)), () => exit3));
    if (isWinner && options?.onWinner) {
      options.onWinner({
        fiber: fiber3,
        index: i,
        parentFiber: parent
      });
    }
  };
  for (let i = 0; i < len; i++) {
    const fiber3 = forkUnsafe(parent, effects[i], true, true, false);
    fibers.add(fiber3);
    fiber3.addObserver((exit3) => {
      fibers.delete(fiber3);
      onExit3(exit3, fiber3, i);
    });
    if (done4) break;
  }
  return fiberInterruptAll(fibers);
}));
var raceAllFirst = (all3, options) => withFiber((parent) => callback((resume) => {
  let done4 = false;
  const fibers = /* @__PURE__ */ new Set();
  const onExit3 = (exit3) => {
    done4 = true;
    resume(fibers.size === 0 ? exit3 : flatMap(uninterruptible(fiberInterruptAll(fibers)), () => exit3));
  };
  let i = 0;
  for (const effect2 of all3) {
    if (done4) break;
    const index = i++;
    const fiber3 = forkUnsafe(parent, effect2, true, true, false);
    fibers.add(fiber3);
    fiber3.addObserver((exit3) => {
      fibers.delete(fiber3);
      const isWinner = !done4;
      onExit3(exit3);
      if (isWinner && options?.onWinner) {
        options.onWinner({
          fiber: fiber3,
          index,
          parentFiber: parent
        });
      }
    });
  }
  return fiberInterruptAll(fibers);
}));
var race = /* @__PURE__ */ dual((args3) => isEffect(args3[1]), (self, that, options) => raceAll([self, that], options));
var raceFirst = /* @__PURE__ */ dual((args3) => isEffect(args3[1]), (self, that, options) => raceAllFirst([self, that], options));
var flatMap = /* @__PURE__ */ dual(2, (self, f) => {
  const onSuccess = Object.create(OnSuccessProto);
  onSuccess[args] = self;
  onSuccess[contA] = f.length !== 1 ? (a) => f(a) : f;
  return onSuccess;
});
var OnSuccessProto = /* @__PURE__ */ makePrimitiveProto({
  op: "OnSuccess",
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args];
  }
});
var matchCauseEffectEager = /* @__PURE__ */ dual(2, (self, options) => {
  if (effectIsExit(self)) {
    return self._tag === "Success" ? options.onSuccess(self.value) : options.onFailure(self.cause);
  }
  return matchCauseEffect(self, options);
});
var effectIsExit = (effect2) => ExitTypeId in effect2;
var flatMapEager = /* @__PURE__ */ dual(2, (self, f) => {
  if (effectIsExit(self)) {
    return self._tag === "Success" ? f(self.value) : self;
  }
  return flatMap(self, f);
});
var flatten = (self) => flatMap(self, identity);
var map = /* @__PURE__ */ dual(2, (self, f) => flatMap(self, (a) => succeed3(internalCall(() => f(a)))));
var mapEager = /* @__PURE__ */ dual(2, (self, f) => effectIsExit(self) ? exitMap(self, f) : map(self, f));
var mapErrorEager = /* @__PURE__ */ dual(2, (self, f) => effectIsExit(self) ? exitMapError(self, f) : mapError2(self, f));
var mapBothEager = /* @__PURE__ */ dual(2, (self, options) => effectIsExit(self) ? exitMapBoth(self, options) : mapBoth(self, options));
var catchEager = /* @__PURE__ */ dual(2, (self, f) => {
  if (effectIsExit(self)) {
    if (self._tag === "Success") return self;
    const error = findError(self.cause);
    if (isFailure2(error)) return self;
    return f(error.success);
  }
  return catch_(self, f);
});
var exitIsSuccess = (self) => self._tag === "Success";
var exitFilterCause = (self) => self._tag === "Failure" ? succeed2(self.cause) : fail2(self);
var exitVoid = /* @__PURE__ */ exitSucceed(void 0);
var exitMap = /* @__PURE__ */ dual(2, (self, f) => self._tag === "Success" ? exitSucceed(f(self.value)) : self);
var exitMapError = /* @__PURE__ */ dual(2, (self, f) => {
  if (self._tag === "Success") return self;
  const error = findError(self.cause);
  if (isFailure2(error)) return self;
  return exitFail(f(error.success));
});
var exitMapBoth = /* @__PURE__ */ dual(2, (self, options) => {
  if (self._tag === "Success") return exitSucceed(options.onSuccess(self.value));
  const error = findError(self.cause);
  if (isFailure2(error)) return self;
  return exitFail(options.onFailure(error.success));
});
var exitAsVoidAll = (exits) => {
  const failures = [];
  for (const exit3 of exits) {
    if (exit3._tag === "Failure") {
      failures.push(...exit3.cause.reasons);
    }
  }
  return failures.length === 0 ? exitVoid : exitFailCause(causeFromReasons(failures));
};
var service = fromYieldable;
var serviceOption = (service3) => withFiber((fiber3) => succeed3(getOption(fiber3.services, service3)));
var serviceOptional = (service3) => withFiber((fiber3) => fiber3.services.mapUnsafe.has(service3.key) ? succeed3(getUnsafe2(fiber3.services, service3)) : fail3(new NoSuchElementError()));
var updateServices = /* @__PURE__ */ dual(2, (self, f) => withFiber((fiber3) => {
  const prev = fiber3.services;
  const nextServices = f(prev);
  if (prev === nextServices) return self;
  fiber3.setServices(nextServices);
  const newServices = /* @__PURE__ */ new Map();
  for (const [key, value] of fiber3.services.mapUnsafe) {
    if (!prev.mapUnsafe.has(key) || value !== prev.mapUnsafe.get(key)) {
      newServices.set(key, value);
    }
  }
  return onExitPrimitive(self, () => {
    const map4 = new Map(fiber3.services.mapUnsafe);
    for (const [key, value] of newServices) {
      if (value !== map4.get(key)) continue;
      if (prev.mapUnsafe.has(key)) {
        map4.set(key, prev.mapUnsafe.get(key));
      } else {
        map4.delete(key);
      }
    }
    fiber3.setServices(makeUnsafe(map4));
    return void 0;
  });
}));
var updateService = /* @__PURE__ */ dual(3, (self, service3, f) => withFiber((fiber3) => {
  const prev = getUnsafe2(fiber3.services, service3);
  const next = f(prev);
  if (prev === next) return self;
  fiber3.setServices(add(fiber3.services, service3, next));
  return onExit(self, () => sync(() => fiber3.setServices(add(fiber3.services, service3, prev))));
}));
var services = () => getServiceMap;
var getServiceMap = /* @__PURE__ */ withFiber((fiber3) => succeed3(fiber3.services));
var servicesWith = (f) => withFiber((fiber3) => f(fiber3.services));
var provideServices = /* @__PURE__ */ dual(2, (self, services3) => {
  if (effectIsExit(self)) return self;
  return updateServices(self, merge(services3));
});
var provideService = function() {
  if (arguments.length === 1) {
    return dual(2, (self, impl) => provideServiceImpl(self, arguments[0], impl));
  }
  return dual(3, (self, service3, impl) => provideServiceImpl(self, service3, impl)).apply(this, arguments);
};
var provideServiceImpl = (self, service3, implementation) => withFiber((fiber3) => {
  const prev = getOption(fiber3.services, service3);
  if (prev._tag === "Some" && prev.value === implementation) return self;
  fiber3.setServices(add(fiber3.services, service3, implementation));
  return onExit(self, () => sync(() => fiber3.setServices(addOrOmit(fiber3.services, service3, prev))));
});
var provideServiceEffect = /* @__PURE__ */ dual(3, (self, service3, acquire) => flatMap(acquire, (implementation) => provideService(self, service3, implementation)));
var withConcurrency = /* @__PURE__ */ provideService(CurrentConcurrency);
var zip = /* @__PURE__ */ dual((args3) => isEffect(args3[1]), (self, that, options) => zipWith(self, that, (a, a2) => [a, a2], options));
var zipWith = /* @__PURE__ */ dual((args3) => isEffect(args3[1]), (self, that, f, options) => options?.concurrent ? map(all([self, that], {
  concurrency: 2
}), ([a, a2]) => internalCall(() => f(a, a2))) : flatMap(self, (a) => map(that, (a2) => internalCall(() => f(a, a2)))));
var filterOrFail = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, filter4, orFailWith) => filterOrElse(self, filter4, orFailWith ? (a) => fail3(orFailWith(a)) : () => fail3(new NoSuchElementError())));
var when = /* @__PURE__ */ dual(2, (self, condition) => flatMap(condition, (pass) => pass ? asSome(self) : succeedNone));
var replicate = /* @__PURE__ */ dual(2, (self, n) => Array.from({
  length: n
}, () => self));
var replicateEffect = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, n, options) => all(replicate(self, n), options));
var forever = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, options) => whileLoop({
  while: constTrue,
  body: constant(options?.disableYield ? self : flatMap(self, (_) => yieldNow)),
  step: constVoid
}));
var catchCause = /* @__PURE__ */ dual(2, (self, f) => {
  const onFailure = Object.create(OnFailureProto);
  onFailure[args] = self;
  onFailure[contE] = f.length !== 1 ? (cause) => f(cause) : f;
  return onFailure;
});
var OnFailureProto = /* @__PURE__ */ makePrimitiveProto({
  op: "OnFailure",
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args];
  }
});
var catchCauseIf = /* @__PURE__ */ dual(3, (self, filter4, f) => catchCause(self, (cause) => {
  const eb = apply(filter4, cause);
  return !isFailure2(eb) ? internalCall(() => f(eb.success, cause)) : failCause(eb.failure);
}));
var catch_ = /* @__PURE__ */ dual(2, (self, f) => catchCauseIf(self, findError, (e) => f(e)));
var catchNoSuchElement = (self) => matchEffect(self, {
  onFailure: (error) => isNoSuchElementError(error) ? succeedNone : fail3(error),
  onSuccess: succeedSome
});
var catchDefect = /* @__PURE__ */ dual(2, (self, f) => catchCauseIf(self, findDefect, f));
var tapCause = /* @__PURE__ */ dual(2, (self, f) => catchCause(self, (cause) => andThen(internalCall(() => f(cause)), failCause(cause))));
var tapCauseIf = /* @__PURE__ */ dual(3, (self, filter4, f) => catchCauseIf(self, (cause) => {
  const result3 = apply(filter4, cause);
  return isFailure2(result3) ? fail2(cause) : result3;
}, (failure, cause) => andThen(internalCall(() => f(failure, cause)), failCause(cause))));
var tapError = /* @__PURE__ */ dual(2, (self, f) => tapCauseIf(self, findError, (e) => f(e)));
var tapErrorTag = /* @__PURE__ */ dual(3, (self, k, f) => {
  const predicate = Array.isArray(k) ? (e) => hasProperty(e, "_tag") && k.includes(e._tag) : isTagged(k);
  return tapError(self, (error) => predicate(error) ? f(error) : void_);
});
var tapDefect = /* @__PURE__ */ dual(2, (self, f) => tapCauseIf(self, findDefect, (_) => f(_)));
var catchIf = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, filter4, f, orElse) => catchCause(self, (cause) => {
  const error = findError(cause);
  if (isFailure2(error)) return failCause(error.failure);
  const result3 = apply(filter4, error.success);
  if (isFailure2(result3)) {
    return orElse ? internalCall(() => orElse(result3.failure)) : failCause(cause);
  }
  return internalCall(() => f(result3.success));
}));
var catchTag = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, k, f, orElse) => {
  const pred = Array.isArray(k) ? (e) => hasProperty(e, "_tag") && k.includes(e._tag) : isTagged(k);
  return catchIf(self, pred, f, orElse);
});
var catchTags = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, cases, orElse) => {
  let keys;
  return catchIf(self, (e) => {
    keys ??= Object.keys(cases);
    return hasProperty(e, "_tag") && isString(e["_tag"]) && keys.includes(e["_tag"]) ? succeed2(e) : fail2(e);
  }, (e) => internalCall(() => cases[e["_tag"]](e)), orElse);
});
var catchReason = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, errorTag, reasonTag, f, orElse) => catchIf(self, (e) => isTagged(e, errorTag) && hasProperty(e, "reason"), (e) => {
  const reason = e.reason;
  if (isTagged(reason, reasonTag)) return f(reason);
  return orElse ? internalCall(() => orElse(reason)) : fail3(e);
}));
var catchReasons = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, errorTag, cases, orElse) => {
  let keys;
  return catchIf(self, (e) => isTagged(e, errorTag) && hasProperty(e, "reason") && hasProperty(e.reason, "_tag") && isString(e.reason._tag), (e) => {
    const reason = e.reason;
    keys ??= Object.keys(cases);
    if (keys.includes(reason._tag)) {
      return internalCall(() => cases[reason._tag](reason));
    }
    return orElse ? internalCall(() => orElse(reason)) : fail3(e);
  });
});
var unwrapReason = /* @__PURE__ */ dual(2, (self, errorTag) => catchIf(self, (e) => {
  if (isTagged(e, errorTag) && hasProperty(e, "reason")) {
    return succeed2(e.reason);
  }
  return fail2(e);
}, fail3));
var mapError2 = /* @__PURE__ */ dual(2, (self, f) => catch_(self, (error) => failSync(() => f(error))));
var mapBoth = /* @__PURE__ */ dual(2, (self, options) => matchEffect(self, {
  onFailure: (e) => failSync(() => options.onFailure(e)),
  onSuccess: (a) => sync(() => options.onSuccess(a))
}));
var orDie = (self) => catch_(self, die);
var orElseSucceed = /* @__PURE__ */ dual(2, (self, f) => catch_(self, (_) => sync(f)));
var eventually = (self) => catch_(self, (_) => flatMap(yieldNow, () => eventually(self)));
var ignore = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, options) => {
  if (!options?.log) {
    return matchEffect(self, {
      onFailure: (_) => void_,
      onSuccess: (_) => void_
    });
  }
  const logEffect = logWithLevel(options.log === true ? void 0 : options.log);
  return matchCauseEffect(self, {
    onFailure(cause) {
      const failure = findFail(cause);
      return isFailure2(failure) ? failCause(failure.failure) : logEffect(cause);
    },
    onSuccess: (_) => void_
  });
});
var ignoreCause = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, options) => {
  if (!options?.log) {
    return matchCauseEffect(self, {
      onFailure: (_) => void_,
      onSuccess: (_) => void_
    });
  }
  const logEffect = logWithLevel(options.log === true ? void 0 : options.log);
  return matchCauseEffect(self, {
    onFailure: logEffect,
    onSuccess: (_) => void_
  });
});
var option = (self) => match2(self, {
  onFailure: none2,
  onSuccess: some2
});
var result = (self) => matchEager(self, {
  onFailure: fail2,
  onSuccess: succeed2
});
var matchCauseEffect = /* @__PURE__ */ dual(2, (self, options) => {
  const primitive = Object.create(OnSuccessAndFailureProto);
  primitive[args] = self;
  primitive[contA] = options.onSuccess.length !== 1 ? (a) => options.onSuccess(a) : options.onSuccess;
  primitive[contE] = options.onFailure.length !== 1 ? (cause) => options.onFailure(cause) : options.onFailure;
  return primitive;
});
var OnSuccessAndFailureProto = /* @__PURE__ */ makePrimitiveProto({
  op: "OnSuccessAndFailure",
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args];
  }
});
var matchCause = /* @__PURE__ */ dual(2, (self, options) => matchCauseEffect(self, {
  onFailure: (cause) => sync(() => options.onFailure(cause)),
  onSuccess: (value) => sync(() => options.onSuccess(value))
}));
var matchEffect = /* @__PURE__ */ dual(2, (self, options) => matchCauseEffect(self, {
  onFailure: (cause) => {
    const fail5 = cause.reasons.find(isFailReason);
    return fail5 ? internalCall(() => options.onFailure(fail5.error)) : failCause(cause);
  },
  onSuccess: options.onSuccess
}));
var match2 = /* @__PURE__ */ dual(2, (self, options) => matchEffect(self, {
  onFailure: (error) => sync(() => options.onFailure(error)),
  onSuccess: (value) => sync(() => options.onSuccess(value))
}));
var matchEager = /* @__PURE__ */ dual(2, (self, options) => {
  if (effectIsExit(self)) {
    if (self._tag === "Success") return exitSucceed(options.onSuccess(self.value));
    const error = findError(self.cause);
    if (isFailure2(error)) return self;
    return exitSucceed(options.onFailure(error.success));
  }
  return match2(self, options);
});
var matchCauseEager = /* @__PURE__ */ dual(2, (self, options) => {
  if (effectIsExit(self)) {
    if (self._tag === "Success") return exitSucceed(options.onSuccess(self.value));
    return exitSucceed(options.onFailure(self.cause));
  }
  return matchCause(self, options);
});
var exit = (self) => effectIsExit(self) ? exitSucceed(self) : exitPrimitive(self);
var exitPrimitive = /* @__PURE__ */ makePrimitive({
  op: "Exit",
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args];
  },
  [contA](value, _, exit3) {
    return succeed3(exit3 ?? exitSucceed(value));
  },
  [contE](cause, _, exit3) {
    return succeed3(exit3 ?? exitFailCause(cause));
  }
});
var isFailure3 = /* @__PURE__ */ matchEager({
  onFailure: () => true,
  onSuccess: () => false
});
var isSuccess3 = /* @__PURE__ */ matchEager({
  onFailure: () => false,
  onSuccess: () => true
});
var delay = /* @__PURE__ */ dual(2, (self, duration) => andThen(sleep(duration), self));
var timeoutOrElse = /* @__PURE__ */ dual(2, (self, options) => raceFirst(self, flatMap(sleep(options.duration), options.onTimeout)));
var timeout = /* @__PURE__ */ dual(2, (self, duration) => timeoutOrElse(self, {
  duration,
  onTimeout: () => fail3(new TimeoutError())
}));
var timeoutOption = /* @__PURE__ */ dual(2, (self, duration) => raceFirst(asSome(self), as(sleep(duration), none2())));
var timed = (self) => clockWith((clock) => {
  const start = clock.currentTimeNanosUnsafe();
  return map(self, (a) => [nanos(clock.currentTimeNanosUnsafe() - start), a]);
});
var ScopeTypeId = "~effect/Scope";
var ScopeCloseableTypeId = "~effect/Scope/Closeable";
var scopeTag = /* @__PURE__ */ Service("effect/Scope");
var scopeClose = (self, exit_) => suspend(() => scopeCloseUnsafe(self, exit_) ?? void_);
var scopeCloseUnsafe = (self, exit_) => {
  if (self.state._tag === "Closed") return;
  const closed = {
    _tag: "Closed",
    exit: exit_
  };
  if (self.state._tag === "Empty") {
    self.state = closed;
    return;
  }
  const {
    finalizers
  } = self.state;
  self.state = closed;
  if (finalizers.size === 0) {
    return;
  } else if (finalizers.size === 1) {
    return finalizers.values().next().value(exit_);
  }
  return scopeCloseFinalizers(self, finalizers, exit_);
};
var scopeCloseFinalizers = /* @__PURE__ */ fnUntraced(function* (self, finalizers, exit_) {
  let exits = [];
  const fibers = [];
  const arr = Array.from(finalizers.values());
  const parent = getCurrentFiber();
  for (let i = arr.length - 1; i >= 0; i--) {
    const finalizer = arr[i];
    if (self.strategy === "sequential") {
      exits.push(yield* exit(finalizer(exit_)));
    } else {
      fibers.push(forkUnsafe(parent, finalizer(exit_), true, true, "inherit"));
    }
  }
  if (fibers.length > 0) {
    exits = yield* fiberAwaitAll(fibers);
  }
  return yield* exitAsVoidAll(exits);
});
var scopeForkUnsafe = (scope3, finalizerStrategy) => {
  const newScope = scopeMakeUnsafe(finalizerStrategy);
  if (scope3.state._tag === "Closed") {
    newScope.state = scope3.state;
    return newScope;
  }
  const key = {};
  scopeAddFinalizerUnsafe(scope3, key, (exit3) => scopeClose(newScope, exit3));
  scopeAddFinalizerUnsafe(newScope, key, (_) => sync(() => scopeRemoveFinalizerUnsafe(scope3, key)));
  return newScope;
};
var scopeAddFinalizerExit = (scope3, finalizer) => {
  return suspend(() => {
    if (scope3.state._tag === "Closed") {
      return finalizer(scope3.state.exit);
    }
    scopeAddFinalizerUnsafe(scope3, {}, finalizer);
    return void_;
  });
};
var scopeAddFinalizerUnsafe = (scope3, key, finalizer) => {
  if (scope3.state._tag === "Empty") {
    scope3.state = {
      _tag: "Open",
      finalizers: /* @__PURE__ */ new Map([[key, finalizer]])
    };
  } else if (scope3.state._tag === "Open") {
    scope3.state.finalizers.set(key, finalizer);
  }
};
var scopeRemoveFinalizerUnsafe = (scope3, key) => {
  if (scope3.state._tag === "Open") {
    scope3.state.finalizers.delete(key);
  }
};
var scopeMakeUnsafe = (finalizerStrategy = "sequential") => ({
  [ScopeCloseableTypeId]: ScopeCloseableTypeId,
  [ScopeTypeId]: ScopeTypeId,
  strategy: finalizerStrategy,
  state: constScopeEmpty
});
var constScopeEmpty = {
  _tag: "Empty"
};
var scope = /* @__PURE__ */ scopeTag.asEffect();
var provideScope = /* @__PURE__ */ provideService(scopeTag);
var scoped = (self) => withFiber((fiber3) => {
  const prev = getOption(fiber3.services, scopeTag);
  const scope3 = scopeMakeUnsafe();
  fiber3.setServices(add(fiber3.services, scopeTag, scope3));
  return onExitPrimitive(self, (exit3) => {
    fiber3.setServices(addOrOmit(fiber3.services, scopeTag, prev));
    return scopeCloseUnsafe(scope3, exit3);
  });
});
var scopedWith = (f) => suspend(() => {
  const scope3 = scopeMakeUnsafe();
  return onExit(f(scope3), (exit3) => suspend(() => scopeCloseUnsafe(scope3, exit3) ?? void_));
});
var acquireRelease = (acquire, release) => uninterruptible(flatMap(scope, (scope3) => tap(acquire, (a) => scopeAddFinalizerExit(scope3, (exit3) => internalCall(() => release(a, exit3))))));
var addFinalizer = (finalizer) => flatMap(scope, (scope3) => servicesWith((services3) => scopeAddFinalizerExit(scope3, (exit3) => provideServices(finalizer(exit3), services3))));
var onExitPrimitive = /* @__PURE__ */ makePrimitive({
  op: "OnExit",
  single: false,
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args][0];
  },
  [contAll](fiber3) {
    if (fiber3.interruptible && this[args][2] !== true) {
      fiber3._stack.push(setInterruptibleTrue);
      fiber3.interruptible = false;
    }
  },
  [contA](value, _, exit3) {
    exit3 ??= exitSucceed(value);
    const eff = this[args][1](exit3);
    return eff ? flatMap(eff, (_2) => exit3) : exit3;
  },
  [contE](cause, _, exit3) {
    exit3 ??= exitFailCause(cause);
    const eff = this[args][1](exit3);
    return eff ? flatMap(eff, (_2) => exit3) : exit3;
  }
});
var onExit = /* @__PURE__ */ dual(2, onExitPrimitive);
var ensuring = /* @__PURE__ */ dual(2, (self, finalizer) => onExit(self, (_) => finalizer));
var onExitIf = /* @__PURE__ */ dual(3, (self, filter4, f) => onExit(self, (exit3) => {
  const pass = apply(filter4, exit3);
  return isFailure2(pass) ? void_ : f(pass.success, exit3);
}));
var onError = /* @__PURE__ */ dual(2, (self, f) => onExitIf(self, exitFilterCause, f));
var onErrorIf = /* @__PURE__ */ dual(3, (self, filter4, f) => onExitIf(self, (exit3) => {
  if (exit3._tag !== "Failure") return fail2(exit3);
  return apply(filter4, exit3.cause);
}, (eb, exit3) => f(eb, exit3.cause)));
var onInterrupt = /* @__PURE__ */ dual(2, (self, finalizer) => onErrorIf(causeFilterInterruptors, finalizer)(self));
var acquireUseRelease = (acquire, use, release) => uninterruptibleMask((restore) => flatMap(acquire, (a) => onExitPrimitive(restore(use(a)), (exit3) => release(a, exit3), true)));
var cachedInvalidateWithTTL = /* @__PURE__ */ dual(2, (self, ttl) => sync(() => {
  const ttlMillis = toMillis(fromInputUnsafe(ttl));
  const isFinite = Number.isFinite(ttlMillis);
  const latch = makeLatchUnsafe(false);
  let expiresAt = 0;
  let running = false;
  let exit3;
  const wait = flatMap(latch.await, () => exit3);
  return [withFiber((fiber3) => {
    const now = isFinite ? fiber3.getRef(ClockRef).currentTimeMillisUnsafe() : 0;
    if (running || now < expiresAt) return exit3 ?? wait;
    running = true;
    latch.closeUnsafe();
    exit3 = void 0;
    return onExit(self, (exit_) => sync(() => {
      running = false;
      expiresAt = now + ttlMillis;
      exit3 = exit_;
      latch.openUnsafe();
    }));
  }), sync(() => {
    expiresAt = 0;
    latch.closeUnsafe();
    exit3 = void 0;
  })];
}));
var cachedWithTTL = /* @__PURE__ */ dual(2, (self, timeToLive) => map(cachedInvalidateWithTTL(self, timeToLive), (tuple) => tuple[0]));
var cached = (self) => cachedWithTTL(self, infinity);
var interrupt = /* @__PURE__ */ withFiber((fiber3) => failCause(causeInterrupt(fiber3.id)));
var uninterruptible = (self) => withFiber((fiber3) => {
  if (!fiber3.interruptible) return self;
  fiber3.interruptible = false;
  fiber3._stack.push(setInterruptibleTrue);
  return self;
});
var setInterruptible = /* @__PURE__ */ makePrimitive({
  op: "SetInterruptible",
  [contAll](fiber3) {
    fiber3.interruptible = this[args];
    if (fiber3._interruptedCause && fiber3.interruptible) {
      return () => failCause(fiber3._interruptedCause);
    }
  }
});
var setInterruptibleTrue = /* @__PURE__ */ setInterruptible(true);
var setInterruptibleFalse = /* @__PURE__ */ setInterruptible(false);
var interruptible = (self) => withFiber((fiber3) => {
  if (fiber3.interruptible) return self;
  fiber3.interruptible = true;
  fiber3._stack.push(setInterruptibleFalse);
  if (fiber3._interruptedCause) return failCause(fiber3._interruptedCause);
  return self;
});
var uninterruptibleMask = (f) => withFiber((fiber3) => {
  if (!fiber3.interruptible) return f(identity);
  fiber3.interruptible = false;
  fiber3._stack.push(setInterruptibleTrue);
  return f(interruptible);
});
var interruptibleMask = (f) => withFiber((fiber3) => {
  if (fiber3.interruptible) return f(identity);
  fiber3.interruptible = true;
  fiber3._stack.push(setInterruptibleFalse);
  return f(uninterruptible);
});
var all = (arg, options) => {
  if (isIterable(arg)) {
    return options?.mode === "result" ? forEach(arg, result, options) : forEach(arg, identity, options);
  } else if (options?.discard) {
    return options.mode === "result" ? forEach(Object.values(arg), result, options) : forEach(Object.values(arg), identity, options);
  }
  return suspend(() => {
    const out = {};
    return as(forEach(Object.entries(arg), ([key, effect2]) => map(options?.mode === "result" ? result(effect2) : effect2, (value) => {
      out[key] = value;
    }), {
      discard: true,
      concurrency: options?.concurrency
    }), out);
  });
};
var partition = /* @__PURE__ */ dual((args3) => isIterable(args3[0]) && !isEffect(args3[0]), (elements, f, options) => map(forEach(elements, (a, i) => result(f(a, i)), options), (results) => partitionMap(results, identity)));
var whileLoop = /* @__PURE__ */ makePrimitive({
  op: "While",
  [contA](value, fiber3) {
    this[args].step(value);
    if (this[args].while()) {
      fiber3._stack.push(this);
      return this[args].body();
    }
    return exitVoid;
  },
  [evaluate](fiber3) {
    if (this[args].while()) {
      fiber3._stack.push(this);
      return this[args].body();
    }
    return exitVoid;
  }
});
var forEach = /* @__PURE__ */ dual((args3) => typeof args3[1] === "function", (iterable, f, options) => withFiber((parent) => {
  const concurrencyOption = options?.concurrency === "inherit" ? parent.getRef(CurrentConcurrency) : options?.concurrency ?? 1;
  const concurrency = concurrencyOption === "unbounded" ? Number.POSITIVE_INFINITY : Math.max(1, concurrencyOption);
  if (concurrency === 1) {
    return forEachSequential(iterable, f, options);
  }
  const items = fromIterable(iterable);
  let length = items.length;
  if (length === 0) {
    return options?.discard ? void_ : succeed3([]);
  }
  const out = options?.discard ? void 0 : new Array(length);
  let index = 0;
  const annotations = fiberStackAnnotations(parent);
  return callback((resume) => {
    const fibers = /* @__PURE__ */ new Set();
    const failures = [];
    let failed = false;
    let inProgress = 0;
    let doneCount = 0;
    let pumping = false;
    let interrupted = false;
    function pump() {
      pumping = true;
      while (inProgress < concurrency && index < length) {
        const currentIndex = index;
        const item = items[currentIndex];
        index++;
        inProgress++;
        try {
          const child = forkUnsafe(parent, f(item, currentIndex), true, true, "inherit");
          fibers.add(child);
          child.addObserver((exit3) => {
            if (interrupted) {
              return;
            }
            fibers.delete(child);
            if (exit3._tag === "Failure") {
              if (!failed) {
                failed = true;
                length = index;
                failures.push(...exit3.cause.reasons);
                fibers.forEach((fiber3) => fiber3.interruptUnsafe(parent.id, annotations));
              } else {
                for (const f2 of exit3.cause.reasons) {
                  if (f2._tag === "Interrupt") continue;
                  failures.push(f2);
                }
              }
            } else if (out !== void 0) {
              out[currentIndex] = exit3.value;
            }
            doneCount++;
            inProgress--;
            if (doneCount === length) {
              resume(failures.length > 0 ? exitFailCause(causeFromReasons(failures)) : succeed3(out));
            } else if (!pumping && !failed && inProgress < concurrency) {
              pump();
            }
          });
        } catch (err) {
          failed = true;
          length = index;
          failures.push(new Die(err));
          fibers.forEach((fiber3) => fiber3.interruptUnsafe(parent.id, annotations));
        }
      }
      pumping = false;
    }
    pump();
    return suspend(() => {
      interrupted = true;
      index = length;
      return fiberInterruptAll(fibers);
    });
  });
}));
var forEachSequential = (iterable, f, options) => suspend(() => {
  const out = options?.discard ? void 0 : [];
  const iterator = iterable[Symbol.iterator]();
  let state = iterator.next();
  let index = 0;
  return as(whileLoop({
    while: () => !state.done,
    body: () => f(state.value, index++),
    step: (b) => {
      if (out) out.push(b);
      state = iterator.next();
    }
  }), out);
});
var filterOrElse = /* @__PURE__ */ dual(3, (self, filter4, orElse) => flatMap(self, (a) => {
  const result3 = apply(filter4, a);
  return isFailure2(result3) ? orElse(result3.failure) : succeed3(result3.success);
}));
var filter2 = /* @__PURE__ */ dual((args3) => isIterable(args3[0]) && !isEffect(args3[0]), (elements, filter4, options) => suspend(() => {
  const out = [];
  return as(forEach(elements, (a, i) => {
    const result3 = filter4(a, i);
    if (typeof result3 === "boolean") {
      if (result3) out.push(a);
      return void_;
    }
    if (!isEffect(result3)) {
      if (!isFailure2(result3)) {
        out.push(result3.success);
      }
      return void_;
    }
    return map(result3, (r) => {
      if (typeof r === "boolean") {
        if (r) out.push(a);
      } else if (!isFailure2(r)) {
        out.push(r.success);
      }
    });
  }, {
    discard: true,
    concurrency: options?.concurrency
  }), out);
}));
var forkChild = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, options) => withFiber((fiber3) => {
  interruptChildrenPatch();
  return succeed3(forkUnsafe(fiber3, self, options?.startImmediately, false, options?.uninterruptible ?? false));
}));
var forkUnsafe = (parent, effect2, immediate = false, daemon = false, uninterruptible3 = false) => {
  const interruptible3 = uninterruptible3 === "inherit" ? parent.interruptible : !uninterruptible3;
  const child = new FiberImpl(parent.services, interruptible3);
  if (immediate) {
    child.evaluate(effect2);
  } else {
    parent.currentScheduler.scheduleTask(() => child.evaluate(effect2), 0);
  }
  if (!daemon && !child._exit) {
    parent.children().add(child);
    child.addObserver(() => parent._children.delete(child));
  }
  return child;
};
var forkDetach = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, options) => withFiber((fiber3) => succeed3(forkUnsafe(fiber3, self, options?.startImmediately, true, options?.uninterruptible))));
var awaitAllChildren = (self) => withFiber((fiber3) => {
  const initialChildren = fiber3._children && fromIterable(fiber3._children);
  return onExit(self, (_) => {
    let children = fiber3._children;
    if (children === void 0 || children.size === 0) {
      return void_;
    } else if (initialChildren) {
      children = filter(children, (child) => !initialChildren.includes(child));
    }
    return asVoid(fiberAwaitAll(children));
  });
});
var forkIn = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, scope3, options) => withFiber((parent) => {
  const fiber3 = forkUnsafe(parent, self, options?.startImmediately, true, options?.uninterruptible);
  if (!fiber3._exit) {
    if (scope3.state._tag !== "Closed") {
      const key = {};
      const finalizer = () => withFiberId((interruptor) => interruptor === fiber3.id ? void_ : fiberInterrupt(fiber3));
      scopeAddFinalizerUnsafe(scope3, key, finalizer);
      fiber3.addObserver(() => scopeRemoveFinalizerUnsafe(scope3, key));
    } else {
      fiber3.interruptUnsafe(parent.id, fiberStackAnnotations(parent));
    }
  }
  return succeed3(fiber3);
}));
var forkScoped = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, options) => flatMap(scope, (scope3) => forkIn(self, scope3, options)));
var runForkWith = (services3) => (effect2, options) => {
  const scheduler = options?.scheduler || !services3.mapUnsafe.has(Scheduler.key) && new MixedScheduler();
  const fiber3 = new FiberImpl(scheduler ? add(services3, Scheduler, scheduler) : services3, options?.uninterruptible !== true);
  fiber3.evaluate(effect2);
  if (fiber3._exit) return fiber3;
  if (options?.signal) {
    if (options.signal.aborted) {
      fiber3.interruptUnsafe();
    } else {
      const abort = () => fiber3.interruptUnsafe();
      options.signal.addEventListener("abort", abort, {
        once: true
      });
      fiber3.addObserver(() => options.signal.removeEventListener("abort", abort));
    }
  }
  return fiber3;
};
var runFork = /* @__PURE__ */ runForkWith(/* @__PURE__ */ empty());
var runCallbackWith = (services3) => {
  const runFork3 = runForkWith(services3);
  return (effect2, options) => {
    const fiber3 = runFork3(effect2, options);
    if (options?.onExit) {
      fiber3.addObserver(options.onExit);
    }
    return (interruptor) => {
      return fiber3.interruptUnsafe(interruptor);
    };
  };
};
var runCallback = /* @__PURE__ */ runCallbackWith(/* @__PURE__ */ empty());
var runPromiseExitWith = (services3) => {
  const runFork3 = runForkWith(services3);
  return (effect2, options) => {
    const fiber3 = runFork3(effect2, options);
    return new Promise((resolve) => {
      fiber3.addObserver((exit3) => resolve(exit3));
    });
  };
};
var runPromiseExit = /* @__PURE__ */ runPromiseExitWith(/* @__PURE__ */ empty());
var runPromiseWith = (services3) => {
  const runPromiseExit3 = runPromiseExitWith(services3);
  return (effect2, options) => runPromiseExit3(effect2, options).then((exit3) => {
    if (exit3._tag === "Failure") {
      throw causeSquash(exit3.cause);
    }
    return exit3.value;
  });
};
var runPromise = /* @__PURE__ */ runPromiseWith(/* @__PURE__ */ empty());
var runSyncExitWith = (services3) => {
  const runFork3 = runForkWith(services3);
  return (effect2) => {
    if (effectIsExit(effect2)) return effect2;
    const scheduler = new MixedScheduler("sync");
    const fiber3 = runFork3(effect2, {
      scheduler
    });
    scheduler.flush();
    return fiber3._exit ?? exitDie(fiber3);
  };
};
var runSyncExit = /* @__PURE__ */ runSyncExitWith(/* @__PURE__ */ empty());
var runSyncWith = (services3) => {
  const runSyncExit3 = runSyncExitWith(services3);
  return (effect2) => {
    const exit3 = runSyncExit3(effect2);
    if (exit3._tag === "Failure") throw causeSquash(exit3.cause);
    return exit3.value;
  };
};
var runSync = /* @__PURE__ */ runSyncWith(/* @__PURE__ */ empty());
var succeedTrue = /* @__PURE__ */ succeed3(true);
var succeedFalse = /* @__PURE__ */ succeed3(false);
var Latch = class {
  waiters = [];
  scheduled = false;
  isOpen;
  constructor(isOpen) {
    this.isOpen = isOpen;
  }
  scheduleUnsafe(fiber3) {
    if (this.scheduled || this.waiters.length === 0) {
      return succeedTrue;
    }
    this.scheduled = true;
    fiber3.currentScheduler.scheduleTask(this.flushWaiters, 0);
    return succeedTrue;
  }
  flushWaiters = () => {
    this.scheduled = false;
    const waiters = this.waiters;
    this.waiters = [];
    for (let i = 0; i < waiters.length; i++) {
      waiters[i](exitVoid);
    }
  };
  open = /* @__PURE__ */ withFiber((fiber3) => {
    if (this.isOpen) return succeedFalse;
    this.isOpen = true;
    return this.scheduleUnsafe(fiber3);
  });
  release = /* @__PURE__ */ withFiber((fiber3) => this.open ? succeedFalse : this.scheduleUnsafe(fiber3));
  openUnsafe() {
    if (this.isOpen) return false;
    this.isOpen = true;
    this.flushWaiters();
    return true;
  }
  await = /* @__PURE__ */ callback((resume) => {
    if (this.isOpen) {
      return resume(void_);
    }
    this.waiters.push(resume);
    return sync(() => {
      const index = this.waiters.indexOf(resume);
      if (index !== -1) {
        this.waiters.splice(index, 1);
      }
    });
  });
  closeUnsafe() {
    if (!this.isOpen) return false;
    this.isOpen = false;
    return true;
  }
  close = /* @__PURE__ */ sync(() => this.closeUnsafe());
  whenOpen = (self) => andThen(this.await, self);
};
var makeLatchUnsafe = (open) => new Latch(open ?? false);
var tracer = /* @__PURE__ */ withFiber((fiber3) => succeed3(fiber3.getRef(Tracer)));
var withTracer = /* @__PURE__ */ dual(2, (effect2, tracer3) => provideService(effect2, Tracer, tracer3));
var withTracerEnabled = /* @__PURE__ */ provideService(TracerEnabled);
var withTracerTiming = /* @__PURE__ */ provideService(TracerTimingEnabled);
var bigint02 = /* @__PURE__ */ BigInt(0);
var NoopSpanProto = {
  _tag: "Span",
  spanId: "noop",
  traceId: "noop",
  sampled: false,
  status: {
    _tag: "Ended",
    startTime: bigint02,
    endTime: bigint02,
    exit: exitVoid
  },
  attributes: /* @__PURE__ */ new Map(),
  links: [],
  kind: "internal",
  attribute() {
  },
  event() {
  },
  end() {
  },
  addLinks() {
  }
};
var noopSpan = (options) => Object.assign(Object.create(NoopSpanProto), options);
var filterDisablePropagation = (span2) => {
  if (span2) {
    return get(span2.annotations, DisablePropagation) ? span2._tag === "Span" ? filterDisablePropagation(span2.parent) : void 0 : span2;
  }
};
var makeSpanUnsafe = (fiber3, name, options) => {
  const disablePropagation = !fiber3.getRef(TracerEnabled) || options?.annotations && get(options.annotations, DisablePropagation);
  const parent = options?.parent ?? (options?.root ? void 0 : filterDisablePropagation(fiber3.currentSpan));
  let span2;
  if (disablePropagation) {
    span2 = noopSpan({
      name,
      parent,
      annotations: add(options?.annotations ?? empty(), DisablePropagation, true)
    });
  } else {
    const tracer3 = fiber3.getRef(Tracer);
    const clock = fiber3.getRef(ClockRef);
    const timingEnabled = fiber3.getRef(TracerTimingEnabled);
    const annotationsFromEnv = fiber3.getRef(TracerSpanAnnotations);
    const linksFromEnv = fiber3.getRef(TracerSpanLinks);
    const level = options?.level ?? fiber3.getRef(CurrentTraceLevel);
    const links = options?.links !== void 0 ? [...linksFromEnv, ...options.links] : linksFromEnv.slice();
    span2 = tracer3.span({
      name,
      parent,
      annotations: options?.annotations ?? empty(),
      links,
      startTime: timingEnabled ? clock.currentTimeNanosUnsafe() : 0n,
      kind: options?.kind ?? "internal",
      root: options?.root ?? options?.parent === void 0,
      sampled: options?.sampled ?? (parent?.sampled === false ? false : !isLogLevelGreaterThan(fiber3.getRef(MinimumTraceLevel), level))
    });
    for (const [key, value] of Object.entries(annotationsFromEnv)) {
      span2.attribute(key, value);
    }
    if (options?.attributes !== void 0) {
      for (const [key, value] of Object.entries(options.attributes)) {
        span2.attribute(key, value);
      }
    }
  }
  return span2;
};
var makeSpan = (name, options) => withFiber((fiber3) => succeed3(makeSpanUnsafe(fiber3, name, options)));
var makeSpanScoped = (name, options) => uninterruptible(withFiber((fiber3) => {
  const scope3 = getUnsafe2(fiber3.services, scopeTag);
  const span2 = makeSpanUnsafe(fiber3, name, options ?? {});
  const clock = fiber3.getRef(ClockRef);
  const timingEnabled = fiber3.getRef(TracerTimingEnabled);
  return as(scopeAddFinalizerExit(scope3, (exit3) => endSpan(span2, exit3, clock, timingEnabled)), span2);
}));
var withSpanScoped = function() {
  const dataFirst = typeof arguments[0] !== "string";
  const name = dataFirst ? arguments[1] : arguments[0];
  const options = addSpanStackTrace(dataFirst ? arguments[2] : arguments[1]);
  if (dataFirst) {
    const self = arguments[0];
    return flatMap(makeSpanScoped(name, options), (span2) => withParentSpan(self, span2, options));
  }
  return (self) => flatMap(makeSpanScoped(name, options), (span2) => withParentSpan(self, span2, options));
};
var provideSpanStackFrame = (name, stack) => {
  stack = typeof stack === "function" ? stack : constUndefined;
  return updateService(CurrentStackFrame, (parent) => ({
    name,
    stack,
    parent
  }));
};
var spanAnnotations = /* @__PURE__ */ TracerSpanAnnotations.asEffect();
var spanLinks = /* @__PURE__ */ TracerSpanLinks.asEffect();
var linkSpans = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, span2, attributes = {}) => {
  const spans = Array.isArray(span2) ? span2 : [span2];
  const links = spans.map((span3) => ({
    span: span3,
    attributes
  }));
  return updateService(self, TracerSpanLinks, (current) => [...current, ...links]);
});
var endSpan = (span2, exit3, clock, timingEnabled) => sync(() => {
  if (span2.status._tag === "Ended") return;
  span2.end(timingEnabled ? clock.currentTimeNanosUnsafe() : bigint02, exit3);
});
var useSpan = (name, ...args3) => {
  const options = args3.length === 1 ? void 0 : args3[0];
  const evaluate2 = args3[args3.length - 1];
  return withFiber((fiber3) => {
    const span2 = makeSpanUnsafe(fiber3, name, options);
    const clock = fiber3.getRef(ClockRef);
    return onExit(internalCall(() => evaluate2(span2)), (exit3) => sync(() => {
      if (span2.status._tag === "Ended") return;
      span2.end(clock.currentTimeNanosUnsafe(), exit3);
    }));
  });
};
var provideParentSpan = /* @__PURE__ */ provideService(ParentSpan);
var withParentSpan = function() {
  const dataFirst = isEffect(arguments[0]);
  const span2 = dataFirst ? arguments[1] : arguments[0];
  let options = dataFirst ? arguments[2] : arguments[1];
  let provideStackFrame = identity;
  if (span2._tag === "Span") {
    options = addSpanStackTrace(options);
    provideStackFrame = provideSpanStackFrame(span2.name, options?.captureStackTrace);
  }
  if (dataFirst) {
    return provideParentSpan(provideStackFrame(arguments[0]), span2);
  }
  return (self) => provideParentSpan(provideStackFrame(self), span2);
};
var withSpan = function() {
  const dataFirst = typeof arguments[0] !== "string";
  const name = dataFirst ? arguments[1] : arguments[0];
  const traceOptions = addSpanStackTrace(arguments[2]);
  if (dataFirst) {
    const self = arguments[0];
    return useSpan(name, arguments[2], (span2) => withParentSpan(self, span2, traceOptions));
  }
  const fnArg = typeof arguments[1] === "function" ? arguments[1] : void 0;
  const options = fnArg ? void 0 : arguments[1];
  return (self, ...args3) => useSpan(name, fnArg ? fnArg(...args3) : options, (span2) => withParentSpan(self, span2, traceOptions));
};
var annotateSpans = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (effect2, ...args3) => updateService(effect2, TracerSpanAnnotations, (annotations) => {
  const newAnnotations = {
    ...annotations
  };
  if (args3.length === 1) {
    Object.assign(newAnnotations, args3[0]);
  } else {
    newAnnotations[args3[0]] = args3[1];
  }
  return newAnnotations;
}));
var annotateCurrentSpan = (...args3) => withFiber((fiber3) => {
  const span2 = fiber3.currentSpanLocal;
  if (span2) {
    if (args3.length === 1) {
      for (const [key, value] of Object.entries(args3[0])) {
        span2.attribute(key, value);
      }
    } else {
      span2.attribute(args3[0], args3[1]);
    }
  }
  return void_;
});
var currentSpan = /* @__PURE__ */ withFiber((fiber3) => {
  const span2 = fiber3.currentSpanLocal;
  return span2 ? succeed3(span2) : fail3(new NoSuchElementError());
});
var currentParentSpan = /* @__PURE__ */ serviceOptional(ParentSpan);
var ClockRef = /* @__PURE__ */ Reference("effect/Clock", {
  defaultValue: () => new ClockImpl()
});
var MAX_TIMER_MILLIS = 2 ** 31 - 1;
var ClockImpl = class {
  currentTimeMillisUnsafe() {
    return Date.now();
  }
  currentTimeMillis = /* @__PURE__ */ sync(() => this.currentTimeMillisUnsafe());
  currentTimeNanosUnsafe() {
    return processOrPerformanceNow();
  }
  currentTimeNanos = /* @__PURE__ */ sync(() => this.currentTimeNanosUnsafe());
  sleep(duration) {
    const millis2 = toMillis(duration);
    if (millis2 <= 0) return yieldNow;
    return callback((resume) => {
      if (millis2 > MAX_TIMER_MILLIS) return;
      const handle = setTimeout(() => resume(void_), millis2);
      return sync(() => clearTimeout(handle));
    });
  }
};
var performanceNowNanos = /* @__PURE__ */ (function() {
  const bigint1e6 = /* @__PURE__ */ BigInt(1e6);
  if (typeof performance === "undefined" || typeof performance.now === "undefined") {
    return () => BigInt(Date.now()) * bigint1e6;
  } else if (typeof performance.timeOrigin === "number" && performance.timeOrigin === 0) {
    return () => BigInt(Math.round(performance.now() * 1e6));
  }
  const origin = /* @__PURE__ */ BigInt(/* @__PURE__ */ Date.now()) * bigint1e6 - /* @__PURE__ */ BigInt(/* @__PURE__ */ Math.round(/* @__PURE__ */ performance.now() * 1e6));
  return () => origin + BigInt(Math.round(performance.now() * 1e6));
})();
var processOrPerformanceNow = /* @__PURE__ */ (function() {
  const processHrtime = typeof process === "object" && "hrtime" in process && typeof process.hrtime.bigint === "function" ? process.hrtime : void 0;
  if (!processHrtime) {
    return performanceNowNanos;
  }
  const origin = /* @__PURE__ */ performanceNowNanos() - /* @__PURE__ */ processHrtime.bigint();
  return () => origin + processHrtime.bigint();
})();
var clockWith = (f) => withFiber((fiber3) => f(fiber3.getRef(ClockRef)));
var sleep = (duration) => clockWith((clock) => clock.sleep(fromInputUnsafe(duration)));
var currentTimeMillis = /* @__PURE__ */ clockWith((clock) => clock.currentTimeMillis);
var TimeoutErrorTypeId = "~effect/Cause/TimeoutError";
var TimeoutError = class extends (/* @__PURE__ */ TaggedError("TimeoutError")) {
  [TimeoutErrorTypeId] = TimeoutErrorTypeId;
  constructor(message) {
    super({
      message
    });
  }
};
var IllegalArgumentErrorTypeId = "~effect/Cause/IllegalArgumentError";
var IllegalArgumentError = class extends (/* @__PURE__ */ TaggedError("IllegalArgumentError")) {
  [IllegalArgumentErrorTypeId] = IllegalArgumentErrorTypeId;
  constructor(message) {
    super({
      message
    });
  }
};
var ExceededCapacityErrorTypeId = "~effect/Cause/ExceededCapacityError";
var ExceededCapacityError = class extends (/* @__PURE__ */ TaggedError("ExceededCapacityError")) {
  [ExceededCapacityErrorTypeId] = ExceededCapacityErrorTypeId;
  constructor(message) {
    super({
      message
    });
  }
};
var UnknownErrorTypeId = "~effect/Cause/UnknownError";
var UnknownError = class extends (/* @__PURE__ */ TaggedError("UnknownError")) {
  [UnknownErrorTypeId] = UnknownErrorTypeId;
  constructor(cause, message) {
    super({
      message,
      cause
    });
  }
};
var ConsoleRef = /* @__PURE__ */ Reference("effect/Console/CurrentConsole", {
  defaultValue: () => globalThis.console
});
var logLevelToOrder = (level) => {
  switch (level) {
    case "All":
      return Number.MIN_SAFE_INTEGER;
    case "Fatal":
      return 5e4;
    case "Error":
      return 4e4;
    case "Warn":
      return 3e4;
    case "Info":
      return 2e4;
    case "Debug":
      return 1e4;
    case "Trace":
      return 0;
    case "None":
      return Number.MAX_SAFE_INTEGER;
  }
};
var LogLevelOrder = /* @__PURE__ */ mapInput(Number2, logLevelToOrder);
var isLogLevelGreaterThan = /* @__PURE__ */ isGreaterThan(LogLevelOrder);
var CurrentLoggers = /* @__PURE__ */ Reference("effect/Loggers/CurrentLoggers", {
  defaultValue: () => /* @__PURE__ */ new Set([defaultLogger, tracerLogger])
});
var LogToStderr = /* @__PURE__ */ Reference("effect/Logger/LogToStderr", {
  defaultValue: constFalse
});
var LoggerTypeId = "~effect/Logger";
var LoggerProto = {
  [LoggerTypeId]: {
    _Message: identity,
    _Output: identity
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var loggerMake = (log4) => {
  const self = Object.create(LoggerProto);
  self.log = log4;
  return self;
};
var formatLabel = (key) => key.replace(/[\s="]/g, "_");
var formatLogSpan = (self, now) => {
  const label = formatLabel(self[0]);
  return `${label}=${now - self[1]}ms`;
};
var structuredMessage = (u) => {
  switch (typeof u) {
    case "bigint":
    case "function":
    case "symbol": {
      return String(u);
    }
    default: {
      return toJson(u);
    }
  }
};
var logWithLevel = (level) => (...message) => {
  let cause = void 0;
  for (let i = 0, len = message.length; i < len; i++) {
    const msg = message[i];
    if (isCause(msg)) {
      if (cause) {
        ;
        message.splice(i, 1);
      } else {
        message = message.slice(0, i).concat(message.slice(i + 1));
      }
      cause = cause ? causeFromReasons(cause.reasons.concat(msg.reasons)) : msg;
      i--;
    }
  }
  if (cause === void 0) {
    cause = causeEmpty;
  }
  return withFiber((fiber3) => {
    const logLevel = level ?? fiber3.currentLogLevel;
    if (isLogLevelGreaterThan(fiber3.minimumLogLevel, logLevel)) {
      return void_;
    }
    const clock = fiber3.getRef(ClockRef);
    const loggers = fiber3.getRef(CurrentLoggers);
    if (loggers.size > 0) {
      const date = new Date(clock.currentTimeMillisUnsafe());
      for (const logger of loggers) {
        logger.log({
          cause,
          fiber: fiber3,
          date,
          logLevel,
          message
        });
      }
    }
    return void_;
  });
};
var withColor = (text, ...colors2) => {
  let out = "";
  for (let i = 0; i < colors2.length; i++) {
    out += `\x1B[${colors2[i]}m`;
  }
  return out + text + "\x1B[0m";
};
var withColorNoop = (text, ..._colors) => text;
var colors = {
  bold: "1",
  red: "31",
  green: "32",
  yellow: "33",
  blue: "34",
  cyan: "36",
  white: "37",
  gray: "90",
  black: "30",
  bgBrightRed: "101"
};
var logLevelColors = {
  None: [],
  All: [],
  Trace: [colors.gray],
  Debug: [colors.blue],
  Info: [colors.green],
  Warn: [colors.yellow],
  Error: [colors.red],
  Fatal: [colors.bgBrightRed, colors.black]
};
var logLevelStyle = {
  None: "",
  All: "",
  Trace: "color:gray",
  Debug: "color:blue",
  Info: "color:green",
  Warn: "color:orange",
  Error: "color:red",
  Fatal: "background-color:red;color:white"
};
var defaultDateFormat = (date) => `${date.getHours().toString().padStart(2, "0")}:${date.getMinutes().toString().padStart(2, "0")}:${date.getSeconds().toString().padStart(2, "0")}.${date.getMilliseconds().toString().padStart(3, "0")}`;
var hasProcessStdout = typeof process === "object" && process !== null && typeof process.stdout === "object" && process.stdout !== null;
var processStdoutIsTTY = hasProcessStdout && process.stdout.isTTY === true;
var hasProcessStdoutOrDeno = hasProcessStdout || "Deno" in globalThis;
var consolePretty = (options) => {
  const mode_ = options?.mode ?? "auto";
  const mode = mode_ === "auto" ? hasProcessStdoutOrDeno ? "tty" : "browser" : mode_;
  const isBrowser = mode === "browser";
  const showColors = typeof options?.colors === "boolean" ? options.colors : processStdoutIsTTY || isBrowser;
  const formatDate2 = options?.formatDate ?? defaultDateFormat;
  return isBrowser ? prettyLoggerBrowser({
    colors: showColors,
    formatDate: formatDate2
  }) : prettyLoggerTty({
    colors: showColors,
    formatDate: formatDate2
  });
};
var prettyLoggerTty = (options) => {
  const processIsBun = typeof process === "object" && "isBun" in process && process.isBun === true;
  const color = options.colors && processStdoutIsTTY ? withColor : withColorNoop;
  return loggerMake(({
    cause,
    date,
    fiber: fiber3,
    logLevel,
    message: message_
  }) => {
    const console2 = fiber3.getRef(ConsoleRef);
    const log4 = fiber3.getRef(LogToStderr) ? console2.error : console2.log;
    const message = Array.isArray(message_) ? message_.slice() : [message_];
    let firstLine = color(`[${options.formatDate(date)}]`, colors.white) + ` ${color(logLevel.toUpperCase(), ...logLevelColors[logLevel])} (#${fiber3.id})`;
    const now = date.getTime();
    const spans = fiber3.getRef(CurrentLogSpans);
    for (const span2 of spans) {
      firstLine += " " + formatLogSpan(span2, now);
    }
    firstLine += ":";
    let messageIndex = 0;
    if (message.length > 0) {
      const firstMaybeString = structuredMessage(message[0]);
      if (typeof firstMaybeString === "string") {
        firstLine += " " + color(firstMaybeString, colors.bold, colors.cyan);
        messageIndex++;
      }
    }
    log4(firstLine);
    if (!processIsBun) console2.group();
    if (cause.reasons.length > 0) {
      log4(causePretty(cause));
    }
    if (messageIndex < message.length) {
      for (; messageIndex < message.length; messageIndex++) {
        log4(redact(message[messageIndex]));
      }
    }
    const annotations = fiber3.getRef(CurrentLogAnnotations);
    for (const [key, value] of Object.entries(annotations)) {
      log4(color(`${key}:`, colors.bold, colors.white), redact(value));
    }
    if (!processIsBun) console2.groupEnd();
  });
};
var prettyLoggerBrowser = (options) => {
  const color = options.colors ? "%c" : "";
  return loggerMake(({
    cause,
    date,
    fiber: fiber3,
    logLevel,
    message: message_
  }) => {
    const console2 = fiber3.getRef(ConsoleRef);
    const message = Array.isArray(message_) ? message_.slice() : [message_];
    let firstLine = `${color}[${options.formatDate(date)}]`;
    const firstParams = [];
    if (options.colors) {
      firstParams.push("color:gray");
    }
    firstLine += ` ${color}${logLevel.toUpperCase()}${color} (#${fiber3.id})`;
    if (options.colors) {
      firstParams.push(logLevelStyle[logLevel], "");
    }
    const now = date.getTime();
    const spans = fiber3.getRef(CurrentLogSpans);
    for (const span2 of spans) {
      firstLine += " " + formatLogSpan(span2, now);
    }
    firstLine += ":";
    let messageIndex = 0;
    if (message.length > 0) {
      const firstMaybeString = structuredMessage(message[0]);
      if (typeof firstMaybeString === "string") {
        firstLine += ` ${color}${firstMaybeString}`;
        if (options.colors) {
          firstParams.push("color:deepskyblue");
        }
        messageIndex++;
      }
    }
    console2.groupCollapsed(firstLine, ...firstParams);
    if (cause.reasons.length > 0) {
      console2.error(causePretty(cause));
    }
    if (messageIndex < message.length) {
      for (; messageIndex < message.length; messageIndex++) {
        console2.log(redact(message[messageIndex]));
      }
    }
    const annotations = fiber3.getRef(CurrentLogAnnotations);
    for (const [key, value] of Object.entries(annotations)) {
      const redacted = redact(value);
      if (options.colors) {
        console2.log(`%c${key}:`, "color:gray", redacted);
      } else {
        console2.log(`${key}:`, redacted);
      }
    }
    console2.groupEnd();
  });
};
var defaultLogger = /* @__PURE__ */ loggerMake(({
  cause,
  date,
  fiber: fiber3,
  logLevel,
  message
}) => {
  const message_ = Array.isArray(message) ? message.slice() : [message];
  if (cause.reasons.length > 0) {
    message_.unshift(causePretty(cause));
  }
  const now = date.getTime();
  const spans = fiber3.getRef(CurrentLogSpans);
  let spanString = "";
  for (const span2 of spans) {
    spanString += ` ${formatLogSpan(span2, now)}`;
  }
  const annotations = fiber3.getRef(CurrentLogAnnotations);
  if (Object.keys(annotations).length > 0) {
    message_.push(annotations);
  }
  const console2 = fiber3.getRef(ConsoleRef);
  const log4 = fiber3.getRef(LogToStderr) ? console2.error : console2.log;
  log4(`[${defaultDateFormat(date)}] ${logLevel.toUpperCase()} (#${fiber3.id})${spanString}:`, ...message_);
});
var tracerLogger = /* @__PURE__ */ loggerMake(({
  cause,
  fiber: fiber3,
  logLevel,
  message
}) => {
  const clock = fiber3.getRef(ClockRef);
  const annotations = fiber3.getRef(CurrentLogAnnotations);
  const span2 = fiber3.currentSpan;
  if (span2 === void 0 || span2._tag === "ExternalSpan") return;
  const attributes = {};
  for (const [key, value] of Object.entries(annotations)) {
    attributes[key] = value;
  }
  attributes["effect.fiberId"] = fiber3.id;
  attributes["effect.logLevel"] = logLevel.toUpperCase();
  if (cause.reasons.length > 0) {
    attributes["effect.cause"] = causePretty(cause);
  }
  span2.event(toStringUnknown(Array.isArray(message) && message.length === 1 ? message[0] : message), clock.currentTimeNanosUnsafe(), attributes);
});
function interruptChildrenPatch() {
  fiberMiddleware.interruptChildren ??= fiberInterruptChildren;
}
var undefined_ = /* @__PURE__ */ succeed3(void 0);

// ../../node_modules/effect/dist/Cause.js
var findError2 = findError;
var isDone2 = isDone;
var done2 = done;

// ../../node_modules/effect/dist/Effect.js
var Effect_exports = {};
__export(Effect_exports, {
  Transaction: () => Transaction,
  YieldableClass: () => YieldableClass,
  acquireRelease: () => acquireRelease2,
  acquireUseRelease: () => acquireUseRelease2,
  addFinalizer: () => addFinalizer2,
  all: () => all2,
  andThen: () => andThen2,
  annotateCurrentSpan: () => annotateCurrentSpan2,
  annotateLogs: () => annotateLogs,
  annotateSpans: () => annotateSpans2,
  as: () => as2,
  asSome: () => asSome2,
  asVoid: () => asVoid2,
  atomic: () => atomic,
  atomicWith: () => atomicWith,
  awaitAllChildren: () => awaitAllChildren2,
  cached: () => cached2,
  cachedInvalidateWithTTL: () => cachedInvalidateWithTTL2,
  cachedWithTTL: () => cachedWithTTL2,
  callback: () => callback2,
  catch: () => catch_3,
  catchCause: () => catchCause3,
  catchCauseIf: () => catchCauseIf2,
  catchDefect: () => catchDefect2,
  catchEager: () => catchEager2,
  catchIf: () => catchIf2,
  catchNoSuchElement: () => catchNoSuchElement2,
  catchReason: () => catchReason2,
  catchReasons: () => catchReasons2,
  catchTag: () => catchTag3,
  catchTags: () => catchTags2,
  clockWith: () => clockWith2,
  currentParentSpan: () => currentParentSpan2,
  currentSpan: () => currentSpan2,
  delay: () => delay2,
  die: () => die2,
  effectify: () => effectify,
  ensuring: () => ensuring2,
  eventually: () => eventually2,
  exit: () => exit2,
  fail: () => fail4,
  failCause: () => failCause2,
  failCauseSync: () => failCauseSync2,
  failSync: () => failSync2,
  fiber: () => fiber2,
  fiberId: () => fiberId2,
  filter: () => filter3,
  filterOrElse: () => filterOrElse2,
  filterOrFail: () => filterOrFail2,
  flatMap: () => flatMap3,
  flatMapEager: () => flatMapEager2,
  flatten: () => flatten2,
  flip: () => flip2,
  fn: () => fn2,
  fnUntraced: () => fnUntraced2,
  fnUntracedEager: () => fnUntracedEager2,
  forEach: () => forEach2,
  forever: () => forever3,
  forkChild: () => forkChild2,
  forkDetach: () => forkDetach2,
  forkIn: () => forkIn2,
  forkScoped: () => forkScoped2,
  fromNullishOr: () => fromNullishOr2,
  fromOption: () => fromOption3,
  fromResult: () => fromResult2,
  fromYieldable: () => fromYieldable2,
  gen: () => gen2,
  ignore: () => ignore2,
  ignoreCause: () => ignoreCause2,
  interrupt: () => interrupt2,
  interruptible: () => interruptible2,
  interruptibleMask: () => interruptibleMask2,
  isEffect: () => isEffect2,
  isFailure: () => isFailure4,
  isSuccess: () => isSuccess5,
  linkSpans: () => linkSpans2,
  log: () => log,
  logDebug: () => logDebug,
  logError: () => logError,
  logFatal: () => logFatal,
  logInfo: () => logInfo,
  logTrace: () => logTrace,
  logWarning: () => logWarning,
  logWithLevel: () => logWithLevel2,
  makeSpan: () => makeSpan2,
  makeSpanScoped: () => makeSpanScoped2,
  map: () => map2,
  mapBoth: () => mapBoth2,
  mapBothEager: () => mapBothEager2,
  mapEager: () => mapEager2,
  mapError: () => mapError3,
  mapErrorEager: () => mapErrorEager2,
  match: () => match3,
  matchCause: () => matchCause2,
  matchCauseEager: () => matchCauseEager2,
  matchCauseEffect: () => matchCauseEffect2,
  matchCauseEffectEager: () => matchCauseEffectEager2,
  matchEager: () => matchEager2,
  matchEffect: () => matchEffect3,
  never: () => never2,
  onError: () => onError2,
  onErrorIf: () => onErrorIf2,
  onExit: () => onExit2,
  onExitIf: () => onExitIf2,
  onExitPrimitive: () => onExitPrimitive2,
  onInterrupt: () => onInterrupt2,
  option: () => option2,
  orDie: () => orDie3,
  orElseSucceed: () => orElseSucceed2,
  partition: () => partition2,
  promise: () => promise2,
  provide: () => provide4,
  provideService: () => provideService2,
  provideServiceEffect: () => provideServiceEffect2,
  provideServices: () => provideServices2,
  race: () => race2,
  raceAll: () => raceAll2,
  raceAllFirst: () => raceAllFirst2,
  raceFirst: () => raceFirst2,
  repeat: () => repeat2,
  repeatOrElse: () => repeatOrElse2,
  replicate: () => replicate2,
  replicateEffect: () => replicateEffect2,
  request: () => request2,
  requestUnsafe: () => requestUnsafe2,
  result: () => result2,
  retry: () => retry2,
  retryOrElse: () => retryOrElse2,
  retryTransaction: () => retryTransaction,
  runCallback: () => runCallback2,
  runCallbackWith: () => runCallbackWith2,
  runFork: () => runFork2,
  runForkWith: () => runForkWith2,
  runPromise: () => runPromise2,
  runPromiseExit: () => runPromiseExit2,
  runPromiseExitWith: () => runPromiseExitWith2,
  runPromiseWith: () => runPromiseWith2,
  runSync: () => runSync2,
  runSyncExit: () => runSyncExit2,
  runSyncExitWith: () => runSyncExitWith2,
  runSyncWith: () => runSyncWith2,
  sandbox: () => sandbox2,
  satisfiesErrorType: () => satisfiesErrorType2,
  satisfiesServicesType: () => satisfiesServicesType2,
  satisfiesSuccessType: () => satisfiesSuccessType2,
  schedule: () => schedule,
  scheduleFrom: () => scheduleFrom2,
  scope: () => scope2,
  scoped: () => scoped2,
  scopedWith: () => scopedWith2,
  service: () => service2,
  serviceOption: () => serviceOption2,
  services: () => services2,
  servicesWith: () => servicesWith2,
  sleep: () => sleep2,
  spanAnnotations: () => spanAnnotations2,
  spanLinks: () => spanLinks2,
  succeed: () => succeed5,
  succeedNone: () => succeedNone2,
  succeedSome: () => succeedSome2,
  suspend: () => suspend2,
  sync: () => sync3,
  tap: () => tap2,
  tapCause: () => tapCause2,
  tapCauseIf: () => tapCauseIf2,
  tapDefect: () => tapDefect2,
  tapError: () => tapError2,
  tapErrorTag: () => tapErrorTag2,
  timed: () => timed2,
  timeout: () => timeout2,
  timeoutOption: () => timeoutOption2,
  timeoutOrElse: () => timeoutOrElse2,
  tracer: () => tracer2,
  track: () => track,
  trackDefects: () => trackDefects,
  trackDuration: () => trackDuration,
  trackErrors: () => trackErrors,
  trackSuccesses: () => trackSuccesses,
  transaction: () => transaction,
  transactionWith: () => transactionWith,
  try: () => try_2,
  tryPromise: () => tryPromise2,
  undefined: () => undefined_2,
  uninterruptible: () => uninterruptible2,
  uninterruptibleMask: () => uninterruptibleMask2,
  unwrapReason: () => unwrapReason2,
  updateService: () => updateService3,
  updateServices: () => updateServices2,
  useSpan: () => useSpan2,
  void: () => void_2,
  when: () => when2,
  whileLoop: () => whileLoop2,
  withConcurrency: () => withConcurrency2,
  withExecutionPlan: () => withExecutionPlan2,
  withFiber: () => withFiber2,
  withLogSpan: () => withLogSpan,
  withLogger: () => withLogger,
  withParentSpan: () => withParentSpan3,
  withSpan: () => withSpan3,
  withSpanScoped: () => withSpanScoped2,
  withTracer: () => withTracer2,
  withTracerEnabled: () => withTracerEnabled2,
  withTracerTiming: () => withTracerTiming2,
  yieldNow: () => yieldNow2,
  yieldNowWith: () => yieldNowWith2,
  zip: () => zip2,
  zipWith: () => zipWith2
});

// ../../node_modules/effect/dist/Exit.js
var isSuccess4 = exitIsSuccess;

// ../../node_modules/effect/dist/Layer.js
var Layer_exports = {};
__export(Layer_exports, {
  CurrentMemoMap: () => CurrentMemoMap,
  build: () => build,
  buildWithMemoMap: () => buildWithMemoMap,
  buildWithScope: () => buildWithScope,
  catch: () => catch_2,
  catchCause: () => catchCause2,
  catchTag: () => catchTag2,
  effect: () => effect,
  effectDiscard: () => effectDiscard,
  effectServices: () => effectServices,
  empty: () => empty3,
  flatMap: () => flatMap2,
  fresh: () => fresh,
  fromBuild: () => fromBuild,
  fromBuildMemo: () => fromBuildMemo,
  isLayer: () => isLayer,
  launch: () => launch,
  makeMemoMap: () => makeMemoMap,
  makeMemoMapUnsafe: () => makeMemoMapUnsafe,
  merge: () => merge2,
  mergeAll: () => mergeAll2,
  mock: () => mock,
  orDie: () => orDie2,
  parentSpan: () => parentSpan,
  provide: () => provide2,
  provideMerge: () => provideMerge,
  satisfiesErrorType: () => satisfiesErrorType,
  satisfiesServicesType: () => satisfiesServicesType,
  satisfiesSuccessType: () => satisfiesSuccessType,
  span: () => span,
  succeed: () => succeed4,
  succeedServices: () => succeedServices,
  sync: () => sync2,
  syncServices: () => syncServices,
  unwrap: () => unwrap,
  updateService: () => updateService2,
  withParentSpan: () => withParentSpan2,
  withSpan: () => withSpan2
});

// ../../node_modules/effect/dist/Deferred.js
var TypeId5 = "~effect/Deferred";
var DeferredProto = {
  [TypeId5]: {
    _A: identity,
    _E: identity
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var makeUnsafe2 = () => {
  const self = Object.create(DeferredProto);
  self.resumes = void 0;
  self.effect = void 0;
  return self;
};
var _await = (self) => callback((resume) => {
  if (self.effect) return resume(self.effect);
  self.resumes ??= [];
  self.resumes.push(resume);
  return sync(() => {
    const index = self.resumes.indexOf(resume);
    self.resumes.splice(index, 1);
  });
});
var completeWith = /* @__PURE__ */ dual(2, (self, effect2) => sync(() => doneUnsafe(self, effect2)));
var done3 = completeWith;
var doneUnsafe = (self, effect2) => {
  if (self.effect) return false;
  self.effect = effect2;
  if (self.resumes) {
    for (let i = 0; i < self.resumes.length; i++) {
      self.resumes[i](effect2);
    }
    self.resumes = void 0;
  }
  return true;
};

// ../../node_modules/effect/dist/Scope.js
var Scope = scopeTag;
var makeUnsafe3 = scopeMakeUnsafe;
var provide = provideScope;
var forkUnsafe2 = scopeForkUnsafe;
var close = scopeClose;

// ../../node_modules/effect/dist/Layer.js
var TypeId6 = "~effect/Layer";
var MemoMapTypeId = "~effect/Layer/MemoMap";
var isLayer = (u) => hasProperty(u, TypeId6);
var LayerProto = {
  [TypeId6]: {
    _ROut: identity,
    _E: identity,
    _RIn: identity
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var fromBuildUnsafe = (build2) => {
  const self = Object.create(LayerProto);
  self.build = build2;
  return self;
};
var fromBuild = (build2) => fromBuildUnsafe((memoMap, scope3) => {
  const layerScope = forkUnsafe2(scope3);
  return onExit(build2(memoMap, layerScope), (exit3) => exit3._tag === "Failure" ? close(layerScope, exit3) : void_);
});
var fromBuildMemo = (build2) => {
  const self = fromBuild((memoMap, scope3) => memoMap.getOrElseMemoize(self, scope3, build2));
  return self;
};
var MemoMapImpl = class {
  get [MemoMapTypeId]() {
    return MemoMapTypeId;
  }
  map = /* @__PURE__ */ new Map();
  getOrElseMemoize(layer2, scope3, build2) {
    if (this.map.has(layer2)) {
      const entry2 = this.map.get(layer2);
      entry2.observers++;
      return andThen(scopeAddFinalizerExit(scope3, (exit3) => entry2.finalizer(exit3)), entry2.effect);
    }
    const layerScope = makeUnsafe3();
    const deferred = makeUnsafe2();
    const entry = {
      observers: 1,
      effect: _await(deferred),
      finalizer: (exit3) => suspend(() => {
        entry.observers--;
        if (entry.observers === 0) {
          this.map.delete(layer2);
          return close(layerScope, exit3);
        }
        return void_;
      })
    };
    this.map.set(layer2, entry);
    return scopeAddFinalizerExit(scope3, entry.finalizer).pipe(flatMap(() => build2(this, layerScope)), onExit((exit3) => {
      entry.effect = exit3;
      return done3(deferred, exit3);
    }));
  }
};
var makeMemoMapUnsafe = () => new MemoMapImpl();
var makeMemoMap = /* @__PURE__ */ sync(makeMemoMapUnsafe);
var CurrentMemoMap = class extends (/* @__PURE__ */ Service()("effect/Layer/CurrentMemoMap")) {
  static getOrCreate = /* @__PURE__ */ getOrElse(this, makeMemoMapUnsafe);
};
var buildWithMemoMap = /* @__PURE__ */ dual(3, (self, memoMap, scope3) => provideService(map(self.build(memoMap, scope3), add(CurrentMemoMap, memoMap)), CurrentMemoMap, memoMap));
var build = (self) => withFiber((fiber3) => buildWithMemoMap(self, CurrentMemoMap.getOrCreate(fiber3.services), getUnsafe2(fiber3.services, Scope)));
var buildWithScope = /* @__PURE__ */ dual(2, (self, scope3) => withFiber((fiber3) => buildWithMemoMap(self, CurrentMemoMap.getOrCreate(fiber3.services), scope3)));
var succeed4 = function() {
  if (arguments.length === 1) {
    return (resource) => succeedServices(make3(arguments[0], resource));
  }
  return succeedServices(make3(arguments[0], arguments[1]));
};
var succeedServices = (services3) => fromBuildUnsafe(constant(succeed3(services3)));
var empty3 = /* @__PURE__ */ succeedServices(/* @__PURE__ */ empty());
var sync2 = function() {
  if (arguments.length === 1) {
    return (evaluate2) => syncServices(() => make3(arguments[0], evaluate2()));
  }
  return syncServices(() => make3(arguments[0], arguments[1]()));
};
var syncServices = (evaluate2) => fromBuildMemo(constant(sync(evaluate2)));
var effect = function() {
  if (arguments.length === 1) {
    return (effect2) => effectImpl(arguments[0], effect2);
  }
  return effectImpl(arguments[0], arguments[1]);
};
var effectImpl = (service3, effect2) => effectServices(map(effect2, (value) => make3(service3, value)));
var effectServices = (effect2) => fromBuildMemo((_, scope3) => provide(effect2, scope3));
var effectDiscard = (effect2) => effectServices(as(effect2, empty()));
var unwrap = (self) => {
  const service3 = Service("effect/Layer/unwrap");
  return flatMap2(effect(service3)(self), get(service3));
};
var mergeAllEffect = (layers, memoMap, scope3) => {
  const parentScope = forkUnsafe2(scope3, "parallel");
  return forEach(layers, (layer2) => layer2.build(memoMap, forkUnsafe2(parentScope, "sequential")), {
    concurrency: layers.length
  }).pipe(map((services3) => mergeAll(...services3)));
};
var mergeAll2 = (...layers) => fromBuild((memoMap, scope3) => mergeAllEffect(layers, memoMap, scope3));
var merge2 = /* @__PURE__ */ dual(2, (self, that) => mergeAll2(self, ...Array.isArray(that) ? that : [that]));
var provideWith = (self, that, f) => fromBuild((memoMap, scope3) => flatMap(Array.isArray(that) ? mergeAllEffect(that, memoMap, scope3) : that.build(memoMap, scope3), (context) => self.build(memoMap, scope3).pipe(provideServices(context), map((merged) => f(merged, context)))));
var provide2 = /* @__PURE__ */ dual(2, (self, that) => provideWith(self, that, identity));
var provideMerge = /* @__PURE__ */ dual(2, (self, that) => provideWith(self, that, (self2, that2) => merge(that2, self2)));
var flatMap2 = /* @__PURE__ */ dual(2, (self, f) => fromBuild((memoMap, scope3) => flatMap(self.build(memoMap, scope3), (context) => f(context).build(memoMap, scope3))));
var orDie2 = (self) => fromBuildUnsafe((memoMap, scope3) => orDie(self.build(memoMap, scope3)));
var catch_2 = /* @__PURE__ */ dual(2, (self, onError3) => fromBuildUnsafe((memoMap, scope3) => catch_(self.build(memoMap, scope3), (e) => onError3(e).build(memoMap, scope3))));
var catchTag2 = /* @__PURE__ */ dual(3, (self, k, f) => fromBuildUnsafe((memoMap, scope3) => catchTag(self.build(memoMap, scope3), k, (error) => f(error).build(memoMap, scope3))));
var catchCause2 = /* @__PURE__ */ dual(2, (self, onError3) => fromBuildUnsafe((memoMap, scope3) => catchCause(self.build(memoMap, scope3), (cause) => onError3(cause).build(memoMap, scope3))));
var updateService2 = /* @__PURE__ */ dual(3, (layer2, service3, f) => provide2(layer2, effect(service3)(map(service3.asEffect(), f))));
var fresh = (self) => fromBuildUnsafe((_, scope3) => self.build(makeMemoMapUnsafe(), scope3));
var launch = (self) => scoped(andThen(build(self), never));
var mock = (service3) => (implementation) => succeed4(service3)(new Proxy({
  ...implementation
}, {
  get(target, prop, _receiver) {
    if (prop in target) {
      return target[prop];
    }
    const prevLimit = Error.stackTraceLimit;
    Error.stackTraceLimit = 2;
    const error = new Error(`${service3.key}: Unimplemented method "${prop.toString()}"`);
    Error.stackTraceLimit = prevLimit;
    error.name = "UnimplementedError";
    return makeUnimplemented(error);
  },
  has: constTrue
}));
var makeUnimplemented = (error) => {
  const dead = die(error);
  function unimplemented() {
    return dead;
  }
  Object.assign(unimplemented, dead);
  Object.setPrototypeOf(unimplemented, Object.getPrototypeOf(dead));
  return unimplemented;
};
var satisfiesSuccessType = () => (layer2) => layer2;
var satisfiesErrorType = () => (layer2) => layer2;
var satisfiesServicesType = () => (layer2) => layer2;
var span = (name, options) => {
  options = addSpanStackTrace(options);
  return effect(ParentSpan, options?.onEnd ? tap(makeSpanScoped(name, options), (span2) => addFinalizer((exit3) => options.onEnd(span2, exit3))) : makeSpanScoped(name, options));
};
var parentSpan = (span2) => succeedServices(ParentSpan.serviceMap(span2));
var withSpan2 = function() {
  const dataFirst = typeof arguments[0] !== "string";
  const name = dataFirst ? arguments[1] : arguments[0];
  const options = addSpanStackTrace(dataFirst ? arguments[2] : arguments[1]);
  if (dataFirst) {
    const self = arguments[0];
    return unwrap(map(options?.onEnd !== void 0 ? tap(makeSpanScoped(name, options), (span2) => addFinalizer((exit3) => options.onEnd(span2, exit3))) : makeSpanScoped(name, options), (span2) => withParentSpan2(self, span2)));
  }
  return (self) => unwrap(map(options?.onEnd !== void 0 ? tap(makeSpanScoped(name, options), (span2) => addFinalizer((exit3) => options.onEnd(span2, exit3))) : makeSpanScoped(name, options), (span2) => withParentSpan2(self, span2)));
};
var withParentSpan2 = function() {
  const dataFirst = isLayer(arguments[0]);
  const span2 = dataFirst ? arguments[1] : arguments[0];
  let options = dataFirst ? arguments[2] : arguments[1];
  let provideStackFrame = identity;
  if (span2._tag === "Span") {
    options = addSpanStackTrace(options);
    provideStackFrame = provideSpanStackFrame2(span2.name, options?.captureStackTrace);
  }
  const parentSpanLayer = parentSpan(span2);
  if (dataFirst) {
    return provide2(provideStackFrame(arguments[0]), parentSpanLayer);
  }
  return (self) => provide2(provideStackFrame(self), parentSpanLayer);
};
var provideSpanStackFrame2 = (name, stack) => {
  stack = typeof stack === "function" ? stack : constUndefined;
  return updateService2(CurrentStackFrame, (parent) => ({
    name,
    stack,
    parent
  }));
};

// ../../node_modules/effect/dist/ExecutionPlan.js
var TypeId7 = "~effect/ExecutionPlan";
var Proto2 = {
  [TypeId7]: TypeId7,
  get withRequirements() {
    const self = this;
    return servicesWith((services3) => succeed3(makeProto(self.steps.map((step) => ({
      ...step,
      provide: isLayer(step.provide) ? provide2(step.provide, succeedServices(services3)) : step.provide
    })))));
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var makeProto = (steps) => {
  const self = Object.create(Proto2);
  self.steps = steps;
  return self;
};
var CurrentMetadata = /* @__PURE__ */ Reference("effect/ExecutionPlan/CurrentMetadata", {
  defaultValue: /* @__PURE__ */ constant({
    attempt: 0,
    stepIndex: 0
  })
});

// ../../node_modules/effect/dist/Data.js
var Data_exports = {};
__export(Data_exports, {
  Class: () => Class3,
  Error: () => Error3,
  TaggedClass: () => TaggedClass,
  TaggedError: () => TaggedError2,
  taggedEnum: () => taggedEnum
});
var Class3 = class extends Class {
  constructor(props) {
    super();
    if (props) {
      Object.assign(this, props);
    }
  }
};
var TaggedClass = (tag) => class extends Class3 {
  _tag = tag;
};
var taggedEnum = () => new Proxy({}, {
  get(_target, tag, _receiver) {
    if (tag === "$is") {
      return isTagged;
    } else if (tag === "$match") {
      return taggedMatch;
    }
    return (props) => ({
      _tag: tag,
      ...props
    });
  }
});
function taggedMatch() {
  if (arguments.length === 1) {
    const cases2 = arguments[0];
    return function(value2) {
      return cases2[value2._tag](value2);
    };
  }
  const value = arguments[0];
  const cases = arguments[1];
  return cases[value._tag](value);
}
var Error3 = Error2;
var TaggedError2 = TaggedError;

// ../../node_modules/effect/dist/Pull.js
var catchDone = /* @__PURE__ */ dual(2, (effect2, f) => catchCauseIf(effect2, filterDoneLeftover, (l) => f(l)));
var filterDone = /* @__PURE__ */ composePassthrough(findError2, (e) => isDone2(e) ? succeed2(e) : fail2(e));
var filterDoneLeftover = /* @__PURE__ */ composePassthrough(findError2, (e) => isDone2(e) ? succeed2(e.value) : fail2(e));
var matchEffect2 = /* @__PURE__ */ dual(2, (self, options) => matchCauseEffect(self, {
  onSuccess: options.onSuccess,
  onFailure: (cause) => {
    const halt = filterDone(cause);
    return !isFailure2(halt) ? options.onDone(halt.success.value) : options.onFailure(halt.failure);
  }
}));

// ../../node_modules/effect/dist/Schedule.js
var TypeId8 = "~effect/Schedule";
var CurrentMetadata2 = /* @__PURE__ */ Reference("effect/Schedule/CurrentMetadata", {
  defaultValue: /* @__PURE__ */ constant({
    input: void 0,
    output: void 0,
    duration: zero,
    attempt: 0,
    start: 0,
    now: 0,
    elapsed: 0,
    elapsedSincePrevious: 0
  })
});
var ScheduleProto = {
  [TypeId8]: {
    _Out: identity,
    _In: identity,
    _Env: identity
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var isSchedule = (u) => hasProperty(u, TypeId8);
var fromStep = (step) => {
  const self = Object.create(ScheduleProto);
  self.step = step;
  return self;
};
var metadataFn = () => {
  let n = 0;
  let previous;
  let start;
  return (now, input) => {
    if (start === void 0) start = now;
    const elapsed = now - start;
    const elapsedSincePrevious = previous === void 0 ? 0 : now - previous;
    previous = now;
    return {
      input,
      attempt: ++n,
      start,
      now,
      elapsed,
      elapsedSincePrevious
    };
  };
};
var fromStepWithMetadata = (step) => fromStep(map(step, (f) => {
  const meta = metadataFn();
  return (now, input) => f(meta(now, input));
}));
var toStep = (schedule2) => catchCause(schedule2.step, (cause) => succeed3(() => failCause(cause)));
var toStepWithMetadata = (schedule2) => clockWith((clock) => map(toStep(schedule2), (step) => {
  const metaFn = metadataFn();
  return (input) => suspend(() => {
    const now = clock.currentTimeMillisUnsafe();
    return flatMap(step(now, input), ([output, duration]) => {
      const meta = metaFn(now, input);
      meta.output = output;
      meta.duration = duration;
      return as(sleep(duration), meta);
    });
  });
}));
var passthrough = (self) => fromStep(map(toStep(self), (step) => (now, input) => matchEffect2(step(now, input), {
  onSuccess: (result3) => succeed3([input, result3[1]]),
  onFailure: failCause,
  onDone: () => done2(input)
})));
var recurs = (times) => while_(forever2, ({
  attempt
}) => succeed3(attempt <= times));
var spaced = (duration) => {
  const decoded = fromInputUnsafe(duration);
  return fromStepWithMetadata(succeed3((meta) => succeed3([meta.attempt - 1, decoded])));
};
var while_ = /* @__PURE__ */ dual(2, (self, predicate) => fromStep(map(toStep(self), (step) => {
  const meta = metadataFn();
  return (now, input) => flatMap(step(now, input), (result3) => {
    const [output, duration] = result3;
    return flatMap(predicate({
      ...meta(now, input),
      output,
      duration
    }), (check) => check ? succeed3(result3) : done2(output));
  });
})));
var forever2 = /* @__PURE__ */ spaced(zero);

// ../../node_modules/effect/dist/internal/layer.js
var provideLayer = (self, layer2, options) => scopedWith((scope3) => flatMap(options?.local ? buildWithMemoMap(layer2, makeMemoMapUnsafe(), scope3) : buildWithScope(layer2, scope3), (context) => provideServices(self, context)));
var provide3 = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (self, source, options) => isServiceMap(source) ? provideServices(self, source) : provideLayer(self, Array.isArray(source) ? mergeAll2(...source) : source, options));

// ../../node_modules/effect/dist/internal/schedule.js
var repeatOrElse = /* @__PURE__ */ dual(3, (self, schedule2, orElse) => flatMap(toStepWithMetadata(schedule2), (step) => {
  let meta = CurrentMetadata2.defaultValue();
  return catch_(forever(tap(flatMap(suspend(() => provideService(self, CurrentMetadata2, meta)), step), (meta_) => sync(() => {
    meta = meta_;
  })), {
    disableYield: true
  }), (error) => isDone(error) ? succeed3(error.value) : orElse(error, meta.attempt === 0 ? none2() : some2(meta)));
}));
var retryOrElse = /* @__PURE__ */ dual(3, (self, policy, orElse) => flatMap(toStepWithMetadata(policy), (step) => {
  let meta = CurrentMetadata2.defaultValue();
  let lastError;
  const loop = catch_(suspend(() => provideService(self, CurrentMetadata2, meta)), (error) => {
    lastError = error;
    return flatMap(step(error), (meta_) => {
      meta = meta_;
      return loop;
    });
  });
  return catchDone(loop, (out) => internalCall(() => orElse(lastError, out)));
}));
var repeat = /* @__PURE__ */ dual(2, (self, options) => {
  const schedule2 = typeof options === "function" ? options(identity) : isSchedule(options) ? options : buildFromOptions(options);
  return repeatOrElse(self, schedule2, fail3);
});
var retry = /* @__PURE__ */ dual(2, (self, options) => {
  const schedule2 = typeof options === "function" ? options(identity) : isSchedule(options) ? options : buildFromOptions(options);
  return retryOrElse(self, schedule2, fail3);
});
var scheduleFrom = /* @__PURE__ */ dual(3, (self, initial, schedule2) => flatMap(toStepWithMetadata(schedule2), (step) => {
  let meta = CurrentMetadata2.defaultValue();
  const selfWithMeta = suspend(() => provideService(self, CurrentMetadata2, meta));
  return catch_(flatMap(step(initial), (meta_) => {
    meta = meta_;
    const body = constant(flatMap(selfWithMeta, step));
    return whileLoop({
      while: constTrue,
      body,
      step(meta_2) {
        meta = meta_2;
      }
    });
  }), (error) => isDone(error) ? succeed3(error.value) : fail3(error));
}));
var passthroughForever = /* @__PURE__ */ passthrough(forever2);
var buildFromOptions = (options) => {
  let schedule2 = options.schedule ?? passthroughForever;
  if (options.while) {
    schedule2 = while_(schedule2, ({
      input
    }) => {
      const applied = options.while(input);
      return isEffect(applied) ? applied : succeed3(applied);
    });
  }
  if (options.until) {
    schedule2 = while_(schedule2, ({
      input
    }) => {
      const applied = options.until(input);
      return isEffect(applied) ? map(applied, (b) => !b) : succeed3(!applied);
    });
  }
  if (options.times !== void 0) {
    schedule2 = while_(schedule2, ({
      attempt
    }) => succeed3(attempt <= options.times));
  }
  return schedule2;
};

// ../../node_modules/effect/dist/internal/executionPlan.js
var withExecutionPlan = /* @__PURE__ */ dual(2, (self, plan) => suspend(() => {
  let i = 0;
  let meta = {
    attempt: 0,
    stepIndex: 0
  };
  const provideMeta = provideServiceEffect(CurrentMetadata, sync(() => {
    meta = {
      attempt: meta.attempt + 1,
      stepIndex: i
    };
    return meta;
  }));
  let result3;
  return flatMap(whileLoop({
    while: () => i < plan.steps.length && (result3 === void 0 || isFailure2(result3)),
    body() {
      const step = plan.steps[i];
      let nextEffect = provideMeta(provide3(self, step.provide));
      if (result3) {
        let attempted = false;
        const wrapped = nextEffect;
        nextEffect = suspend(() => {
          if (attempted) return wrapped;
          attempted = true;
          return result3.asEffect();
        });
        nextEffect = retry(nextEffect, scheduleFromStep(step, false));
      } else {
        const schedule2 = scheduleFromStep(step, true);
        nextEffect = schedule2 ? retry(nextEffect, schedule2) : nextEffect;
      }
      return result(nextEffect);
    },
    step(result_) {
      result3 = result_;
      i++;
    }
  }), () => result3.asEffect());
}));
var scheduleFromStep = (step, first) => {
  if (!first) {
    return buildFromOptions({
      schedule: step.schedule ? step.schedule : step.attempts ? void 0 : scheduleOnce,
      times: step.attempts,
      while: step.while
    });
  } else if (step.attempts === 1 || !(step.schedule || step.attempts)) {
    return void 0;
  }
  return buildFromOptions({
    schedule: step.schedule,
    while: step.while,
    times: step.attempts ? step.attempts - 1 : void 0
  });
};
var scheduleOnce = /* @__PURE__ */ recurs(1);

// ../../node_modules/effect/dist/Request.js
var TypeId9 = "~effect/Request";
var requestVariance = /* @__PURE__ */ byReferenceUnsafe({
  /* c8 ignore next */
  _E: (_) => _,
  /* c8 ignore next */
  _A: (_) => _,
  /* c8 ignore next */
  _R: (_) => _
});
var RequestPrototype = {
  ...StructuralProto,
  [TypeId9]: requestVariance
};
var makeEntry = (options) => options;

// ../../node_modules/effect/dist/internal/request.js
var request = /* @__PURE__ */ dual(2, (self, resolver) => {
  const withResolver = (resolver2) => callback((resume) => {
    const entry = addEntry(resolver2, self, resume, getCurrentFiber());
    return maybeRemoveEntry(resolver2, entry);
  });
  return isEffect(resolver) ? flatMap(resolver, withResolver) : withResolver(resolver);
});
var requestUnsafe = (self, options) => {
  const entry = addEntry(options.resolver, self, options.onExit, {
    services: options.services,
    currentScheduler: get(options.services, Scheduler)
  });
  return () => removeEntryUnsafe(options.resolver, entry);
};
var batchPool = [];
var pendingBatches = /* @__PURE__ */ new Map();
var addEntry = (resolver, request3, resume, fiber3) => {
  let batchMap = pendingBatches.get(resolver);
  if (!batchMap) {
    batchMap = /* @__PURE__ */ new Map();
    pendingBatches.set(resolver, batchMap);
  }
  let batch;
  let completed = false;
  const entry = makeEntry({
    request: request3,
    services: fiber3.services,
    uninterruptible: false,
    completeUnsafe(effect2) {
      if (completed) return;
      completed = true;
      resume(effect2);
      batch?.entrySet.delete(entry);
    }
  });
  if (resolver.preCheck !== void 0 && !resolver.preCheck(entry)) {
    return entry;
  }
  const key = resolver.batchKey(entry);
  batch = batchMap.get(key);
  if (!batch) {
    if (batchPool.length > 0) {
      batch = batchPool.pop();
      batch.key = key;
      batch.resolver = resolver;
      batch.map = batchMap;
    } else {
      const newBatch = {
        key,
        resolver,
        map: batchMap,
        entrySet: /* @__PURE__ */ new Set(),
        entries: /* @__PURE__ */ new Set(),
        delayEffect: flatMap(suspend(() => newBatch.resolver.delay), (_) => runBatch(newBatch)),
        run: onExit(suspend(() => newBatch.resolver.runAll(Array.from(newBatch.entries), newBatch.key)), (exit3) => {
          for (const entry2 of newBatch.entrySet) {
            entry2.completeUnsafe(exit3._tag === "Success" ? exitDie(new Error("Effect.request: RequestResolver did not complete request", {
              cause: entry2.request
            })) : exit3);
          }
          newBatch.entries.clear();
          if (batchPool.length < 128) {
            newBatch.entrySet.clear();
            newBatch.key = void 0;
            newBatch.fiber = void 0;
            batchPool.push(newBatch);
          }
          return void_;
        })
      };
      batch = newBatch;
    }
    batchMap.set(key, batch);
    batch.fiber = runFork(batch.delayEffect, {
      scheduler: fiber3.currentScheduler
    });
  }
  batch.entrySet.add(entry);
  batch.entries.add(entry);
  if (batch.resolver.collectWhile(batch.entries)) return entry;
  batch.fiber.interruptUnsafe(fiber3.id);
  batch.fiber = runFork(runBatch(batch), {
    scheduler: fiber3.currentScheduler
  });
  return entry;
};
var removeEntryUnsafe = (resolver, entry) => {
  if (entry.uninterruptible) return;
  const batchMap = pendingBatches.get(resolver);
  if (!batchMap) return;
  const key = resolver.batchKey(entry.request);
  const batch = batchMap.get(key);
  if (!batch) return;
  batch.entries.delete(entry);
  batch.entrySet.delete(entry);
  if (batch.entries.size === 0) {
    batchMap.delete(key);
    batch.fiber?.interruptUnsafe();
  }
};
var maybeRemoveEntry = (resolver, entry) => sync(() => removeEntryUnsafe(resolver, entry));
function runBatch(batch) {
  if (!batch.map.has(batch.key)) return void_;
  batch.map.delete(batch.key);
  return batch.run;
}

// ../../node_modules/effect/dist/Metric.js
var CurrentMetricAttributesKey = "effect/Metric/CurrentMetricAttributes";
var CurrentMetricAttributes = /* @__PURE__ */ Reference(CurrentMetricAttributesKey, {
  defaultValue: () => ({})
});
var MetricRegistryKey = "~effect/observability/Metric/MetricRegistryKey";
var MetricRegistry = /* @__PURE__ */ Reference(MetricRegistryKey, {
  defaultValue: () => /* @__PURE__ */ new Map()
});
var TypeId10 = "~effect/observability/Metric";
var Metric$ = class {
  [TypeId10] = TypeId10;
  #metadataCache = /* @__PURE__ */ new WeakMap();
  #metadata;
  id;
  description;
  attributes;
  constructor(id, description, attributes) {
    this.id = id;
    this.description = description;
    this.attributes = attributes;
  }
  valueUnsafe(context) {
    return this.hook(context).get(context);
  }
  modifyUnsafe(input, context) {
    return this.hook(context).modify(input, context);
  }
  updateUnsafe(input, context) {
    return this.hook(context).update(input, context);
  }
  hook(context) {
    const extraAttributes = get(context, CurrentMetricAttributes);
    if (Object.keys(extraAttributes).length === 0) {
      if (isNotUndefined(this.#metadata)) {
        return this.#metadata.hooks;
      }
      this.#metadata = this.getOrCreate(context, this.attributes);
      return this.#metadata.hooks;
    }
    const mergedAttributes = mergeAttributes(this.attributes, extraAttributes);
    let metadata = this.#metadataCache.get(mergedAttributes);
    if (isNotUndefined(metadata)) {
      return metadata.hooks;
    }
    metadata = this.getOrCreate(context, mergedAttributes);
    this.#metadataCache.set(mergedAttributes, metadata);
    return metadata.hooks;
  }
  getOrCreate(context, attributes) {
    const key = makeKey(this, attributes);
    const registry = get(context, MetricRegistry);
    if (registry.has(key)) {
      return registry.get(key);
    }
    const hooks = this.createHooks();
    const meta = {
      id: this.id,
      type: this.type,
      description: this.description,
      attributes: attributesToRecord(attributes),
      hooks
    };
    registry.set(key, meta);
    return meta;
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var update = /* @__PURE__ */ dual(2, (self, input) => servicesWith((services3) => sync(() => self.updateUnsafe(input, services3))));
function makeKey(metric, attributes) {
  let key = `${metric.type}:${metric.id}`;
  if (isNotUndefined(metric.description)) {
    key += `:${metric.description}`;
  }
  if (isNotUndefined(attributes)) {
    key += `:${serializeAttributes(attributes)}`;
  }
  return key;
}
function serializeAttributes(attributes) {
  return serializeEntries(Array.isArray(attributes) ? attributes : Object.entries(attributes));
}
function serializeEntries(entries) {
  return entries.map(([key, value]) => `${key}=${value}`).join(",");
}
function mergeAttributes(self, other) {
  return {
    ...attributesToRecord(self),
    ...attributesToRecord(other)
  };
}
function attributesToRecord(attributes) {
  if (isNotUndefined(attributes) && Array.isArray(attributes)) {
    return attributes.reduce((acc, [key, value]) => {
      acc[key] = value;
      return acc;
    }, {});
  }
  return attributes;
}

// ../../node_modules/effect/dist/Effect.js
var TypeId11 = EffectTypeId;
var YieldableClass = class {
  [Symbol.iterator]() {
    return new SingleShotGen(this);
  }
};
var isEffect2 = (u) => typeof u === "object" && u !== null && TypeId11 in u;
var all2 = all;
var partition2 = partition;
var forEach2 = forEach;
var whileLoop2 = whileLoop;
var promise2 = promise;
var tryPromise2 = tryPromise;
var succeed5 = succeed3;
var succeedNone2 = succeedNone;
var succeedSome2 = succeedSome;
var suspend2 = suspend;
var sync3 = sync;
var void_2 = void_;
var undefined_2 = undefined_;
var callback2 = callback;
var never2 = never;
var gen2 = gen;
var fail4 = fail3;
var failSync2 = failSync;
var failCause2 = failCause;
var failCauseSync2 = failCauseSync;
var die2 = die;
var try_2 = try_;
var yieldNow2 = yieldNow;
var yieldNowWith2 = yieldNowWith;
var withFiber2 = withFiber;
var fromResult2 = fromResult;
var fromOption3 = fromOption2;
var fromNullishOr2 = fromNullishOr;
var fromYieldable2 = fromYieldable;
var flatMap3 = flatMap;
var flatten2 = flatten;
var andThen2 = andThen;
var tap2 = tap;
var result2 = result;
var option2 = option;
var exit2 = exit;
var map2 = map;
var as2 = as;
var asSome2 = asSome;
var asVoid2 = asVoid;
var flip2 = flip;
var zip2 = zip;
var zipWith2 = zipWith;
var catch_3 = catch_;
var catchTag3 = catchTag;
var catchTags2 = catchTags;
var catchReason2 = catchReason;
var catchReasons2 = catchReasons;
var unwrapReason2 = unwrapReason;
var catchCause3 = catchCause;
var catchDefect2 = catchDefect;
var catchIf2 = catchIf;
var catchNoSuchElement2 = catchNoSuchElement;
var catchCauseIf2 = catchCauseIf;
var mapError3 = mapError2;
var mapBoth2 = mapBoth;
var orDie3 = orDie;
var tapError2 = tapError;
var tapErrorTag2 = tapErrorTag;
var tapCause2 = tapCause;
var tapCauseIf2 = tapCauseIf;
var tapDefect2 = tapDefect;
var eventually2 = eventually;
var retry2 = retry;
var retryOrElse2 = retryOrElse;
var sandbox2 = sandbox;
var ignore2 = ignore;
var ignoreCause2 = ignoreCause;
var withExecutionPlan2 = withExecutionPlan;
var orElseSucceed2 = orElseSucceed;
var timeout2 = timeout;
var timeoutOption2 = timeoutOption;
var timeoutOrElse2 = timeoutOrElse;
var delay2 = delay;
var sleep2 = sleep;
var timed2 = timed;
var raceAll2 = raceAll;
var raceAllFirst2 = raceAllFirst;
var race2 = race;
var raceFirst2 = raceFirst;
var filter3 = filter2;
var filterOrElse2 = filterOrElse;
var filterOrFail2 = filterOrFail;
var when2 = when;
var match3 = match2;
var matchEager2 = matchEager;
var matchCause2 = matchCause;
var matchCauseEager2 = matchCauseEager;
var matchCauseEffectEager2 = matchCauseEffectEager;
var matchCauseEffect2 = matchCauseEffect;
var matchEffect3 = matchEffect;
var isFailure4 = isFailure3;
var isSuccess5 = isSuccess3;
var services2 = services;
var servicesWith2 = servicesWith;
var provide4 = provide3;
var provideServices2 = provideServices;
var service2 = service;
var serviceOption2 = serviceOption;
var updateServices2 = updateServices;
var updateService3 = updateService;
var provideService2 = provideService;
var provideServiceEffect2 = provideServiceEffect;
var withConcurrency2 = withConcurrency;
var scope2 = scope;
var scoped2 = scoped;
var scopedWith2 = scopedWith;
var acquireRelease2 = acquireRelease;
var acquireUseRelease2 = acquireUseRelease;
var addFinalizer2 = addFinalizer;
var ensuring2 = ensuring;
var onError2 = onError;
var onErrorIf2 = onErrorIf;
var onExitPrimitive2 = onExitPrimitive;
var onExit2 = onExit;
var onExitIf2 = onExitIf;
var cached2 = cached;
var cachedWithTTL2 = cachedWithTTL;
var cachedInvalidateWithTTL2 = cachedInvalidateWithTTL;
var interrupt2 = interrupt;
var interruptible2 = interruptible;
var onInterrupt2 = onInterrupt;
var uninterruptible2 = uninterruptible;
var uninterruptibleMask2 = uninterruptibleMask;
var interruptibleMask2 = interruptibleMask;
var forever3 = forever;
var repeat2 = repeat;
var repeatOrElse2 = repeatOrElse;
var replicate2 = replicate;
var replicateEffect2 = replicateEffect;
var schedule = /* @__PURE__ */ dual(2, (self, schedule2) => scheduleFrom2(self, void 0, schedule2));
var scheduleFrom2 = scheduleFrom;
var tracer2 = tracer;
var withTracer2 = withTracer;
var withTracerEnabled2 = withTracerEnabled;
var withTracerTiming2 = withTracerTiming;
var annotateSpans2 = annotateSpans;
var annotateCurrentSpan2 = annotateCurrentSpan;
var currentSpan2 = currentSpan;
var currentParentSpan2 = currentParentSpan;
var spanAnnotations2 = spanAnnotations;
var spanLinks2 = spanLinks;
var linkSpans2 = linkSpans;
var makeSpan2 = makeSpan;
var makeSpanScoped2 = makeSpanScoped;
var useSpan2 = useSpan;
var withSpan3 = withSpan;
var withSpanScoped2 = withSpanScoped;
var withParentSpan3 = withParentSpan;
var request2 = request;
var requestUnsafe2 = requestUnsafe;
var forkChild2 = forkChild;
var forkIn2 = forkIn;
var forkScoped2 = forkScoped;
var forkDetach2 = forkDetach;
var awaitAllChildren2 = awaitAllChildren;
var fiber2 = fiber;
var fiberId2 = fiberId;
var runFork2 = runFork;
var runForkWith2 = runForkWith;
var runCallbackWith2 = runCallbackWith;
var runCallback2 = runCallback;
var runPromise2 = runPromise;
var runPromiseWith2 = runPromiseWith;
var runPromiseExit2 = runPromiseExit;
var runPromiseExitWith2 = runPromiseExitWith;
var runSync2 = runSync;
var runSyncWith2 = runSyncWith;
var runSyncExit2 = runSyncExit;
var runSyncExitWith2 = runSyncExitWith;
var fnUntraced2 = fnUntraced;
var fn2 = fn;
var clockWith2 = clockWith;
var logWithLevel2 = logWithLevel;
var log = /* @__PURE__ */ logWithLevel();
var logFatal = /* @__PURE__ */ logWithLevel("Fatal");
var logWarning = /* @__PURE__ */ logWithLevel("Warn");
var logError = /* @__PURE__ */ logWithLevel("Error");
var logInfo = /* @__PURE__ */ logWithLevel("Info");
var logDebug = /* @__PURE__ */ logWithLevel("Debug");
var logTrace = /* @__PURE__ */ logWithLevel("Trace");
var withLogger = /* @__PURE__ */ dual(2, (effect2, logger) => updateService(effect2, CurrentLoggers, (loggers) => /* @__PURE__ */ new Set([...loggers, logger])));
var annotateLogs = /* @__PURE__ */ dual((args3) => isEffect(args3[0]), (effect2, ...args3) => updateService(effect2, CurrentLogAnnotations, (annotations) => {
  const newAnnotations = {
    ...annotations
  };
  if (args3.length === 1) {
    Object.assign(newAnnotations, args3[0]);
  } else {
    newAnnotations[args3[0]] = args3[1];
  }
  return newAnnotations;
}));
var withLogSpan = /* @__PURE__ */ dual(2, (effect2, label) => flatMap(currentTimeMillis, (now) => updateService(effect2, CurrentLogSpans, (spans) => {
  const span2 = [label, now];
  return [span2, ...spans];
})));
var track = /* @__PURE__ */ dual((args3) => isEffect2(args3[0]), (self, metric, f) => onExit2(self, (exit3) => {
  const input = f === void 0 ? exit3 : internalCall(() => f(exit3));
  return update(metric, input);
}));
var trackSuccesses = /* @__PURE__ */ dual((args3) => isEffect2(args3[0]), (self, metric, f) => tap2(self, (value) => {
  const input = f === void 0 ? value : f(value);
  return update(metric, input);
}));
var trackErrors = /* @__PURE__ */ dual((args3) => isEffect2(args3[0]), (self, metric, f) => tapError2(self, (error) => {
  const input = f === void 0 ? error : internalCall(() => f(error));
  return update(metric, input);
}));
var trackDefects = /* @__PURE__ */ dual((args3) => isEffect2(args3[0]), (self, metric, f) => tapDefect2(self, (defect) => {
  const input = f === void 0 ? defect : internalCall(() => f(defect));
  return update(metric, input);
}));
var trackDuration = /* @__PURE__ */ dual((args3) => isEffect2(args3[0]), (self, metric, f) => clockWith2((clock) => {
  const startTime = clock.currentTimeNanosUnsafe();
  return onExit2(self, () => {
    const endTime = clock.currentTimeNanosUnsafe();
    const duration = subtract(fromInputUnsafe(endTime), fromInputUnsafe(startTime));
    const input = f === void 0 ? duration : internalCall(() => f(duration));
    return update(metric, input);
  });
}));
var Transaction = class extends (/* @__PURE__ */ Service()("effect/Effect/Transaction")) {
};
var atomic = (effect2) => atomicWith(() => effect2);
var atomicWith = (f) => withFiber2((fiber3) => {
  if (fiber3.services.mapUnsafe.has(Transaction.key)) {
    return internalCall(() => f(getUnsafe2(fiber3.services, Transaction)));
  }
  return transactionWith(f);
});
var transaction = (effect2) => transactionWith(() => effect2);
var transactionWith = (f) => withFiber2((fiber3) => {
  const state = {
    journal: /* @__PURE__ */ new Map(),
    retry: false
  };
  const scheduler = fiber3.currentScheduler;
  let result3;
  return uninterruptibleMask2((restore) => flatMap3(whileLoop2({
    while: () => !result3,
    body: constant(restore(suspend2(() => f(state))).pipe(provideService2(Transaction, state), tapCause2(() => {
      if (!state.retry) return void_2;
      return restore(awaitPendingTransaction(state));
    }), exit2)),
    step(exit3) {
      if (state.retry || !isTransactionConsistent(state)) {
        return clearTransaction(state);
      }
      if (isSuccess4(exit3)) {
        commitTransaction(scheduler, state);
      } else {
        clearTransaction(state);
      }
      result3 = exit3;
    }
  }), () => result3));
});
var isTransactionConsistent = (state) => {
  for (const [ref, {
    version: version2
  }] of state.journal) {
    if (ref.version !== version2) {
      return false;
    }
  }
  return true;
};
var awaitPendingTransaction = (state) => suspend2(() => {
  const key = {};
  const refs = Array.from(state.journal.keys());
  const clearPending = () => {
    for (const clear of refs) {
      clear.pending.delete(key);
    }
  };
  return callback2((resume) => {
    const onCall = () => {
      clearPending();
      resume(void_2);
    };
    for (const ref of refs) {
      ref.pending.set(key, onCall);
    }
    return sync3(clearPending);
  });
});
function commitTransaction(scheduler, state) {
  for (const [ref, {
    value
  }] of state.journal) {
    if (value !== ref.value) {
      ref.version = ref.version + 1;
      ref.value = value;
    }
    for (const pending of ref.pending.values()) {
      scheduler.scheduleTask(pending, 0);
    }
    ref.pending.clear();
  }
}
function clearTransaction(state) {
  state.retry = false;
  state.journal.clear();
}
var retryTransaction = /* @__PURE__ */ flatMap3(/* @__PURE__ */ Transaction.asEffect(), (state) => {
  state.retry = true;
  return interrupt2;
});
var effectify = (fn3, onError3, onSyncError) => (...args3) => callback2((resume) => {
  try {
    fn3(...args3, (err, result3) => {
      if (err) {
        resume(fail4(onError3 ? onError3(err, args3) : err));
      } else {
        resume(succeed5(result3));
      }
    });
  } catch (err) {
    resume(onSyncError ? fail4(onSyncError(err, args3)) : die2(err));
  }
});
var satisfiesSuccessType2 = () => (effect2) => effect2;
var satisfiesErrorType2 = () => (effect2) => effect2;
var satisfiesServicesType2 = () => (effect2) => effect2;
var mapEager2 = mapEager;
var mapErrorEager2 = mapErrorEager;
var mapBothEager2 = mapBothEager;
var flatMapEager2 = flatMapEager;
var catchEager2 = catchEager;
var fnUntracedEager2 = fnUntracedEager;

// ../../node_modules/effect/dist/FileSystem.js
var bigint1024 = /* @__PURE__ */ BigInt(1024);
var bigintPiB = bigint1024 * bigint1024 * bigint1024 * bigint1024 * bigint1024;
var FileSystem = /* @__PURE__ */ Service("effect/platform/FileSystem");

// ../../node_modules/effect/dist/Logger.js
var Logger_exports = {};
__export(Logger_exports, {
  CurrentLoggers: () => CurrentLoggers2,
  LogToStderr: () => LogToStderr2,
  batched: () => batched,
  consoleJson: () => consoleJson,
  consoleLogFmt: () => consoleLogFmt,
  consolePretty: () => consolePretty2,
  consoleStructured: () => consoleStructured,
  defaultLogger: () => defaultLogger2,
  formatJson: () => formatJson2,
  formatLogFmt: () => formatLogFmt,
  formatSimple: () => formatSimple,
  formatStructured: () => formatStructured,
  isLogger: () => isLogger,
  layer: () => layer,
  make: () => make5,
  map: () => map3,
  toFile: () => toFile,
  tracerLogger: () => tracerLogger2,
  withConsoleError: () => withConsoleError,
  withConsoleLog: () => withConsoleLog,
  withLeveledConsole: () => withLeveledConsole
});
var TypeId12 = "~effect/Logger";
var isLogger = (u) => hasProperty(u, TypeId12);
var CurrentLoggers2 = CurrentLoggers;
var LogToStderr2 = LogToStderr;
var map3 = /* @__PURE__ */ dual(2, (self, f) => loggerMake((options) => f(self.log(options))));
var withConsoleLog = (self) => loggerMake((options) => {
  const console2 = options.fiber.getRef(ConsoleRef);
  return console2.log(self.log(options));
});
var withConsoleError = (self) => loggerMake((options) => {
  const console2 = options.fiber.getRef(ConsoleRef);
  return console2.error(self.log(options));
});
var withLeveledConsole = (self) => loggerMake((options) => {
  const console2 = options.fiber.getRef(ConsoleRef);
  const output = self.log(options);
  switch (options.logLevel) {
    case "Debug":
      return console2.debug(output);
    case "Info":
      return console2.info(output);
    case "Trace":
      return console2.trace(output);
    case "Warn":
      return console2.warn(output);
    case "Error":
    case "Fatal":
      return console2.error(output);
    default:
      return console2.log(output);
  }
});
var textOnly = /^[^\s"=]*$/;
var escapeDoubleQuotes = (s) => `"${s.replace(/\\([\s\S])|(")/g, "\\$1$2")}"`;
var formatFiberId = (fiberId3) => `#${fiberId3}`;
var format2 = (quoteValue, space) => ({
  cause,
  date,
  fiber: fiber3,
  logLevel,
  message
}) => {
  const formatValue = (value) => value.match(textOnly) ? value : quoteValue(value);
  const format3 = (label, value) => `${formatLabel(label)}=${formatValue(value)}`;
  const append = (label, value) => " " + format3(label, value);
  let out = format3("timestamp", date.toISOString());
  out += append("level", logLevel);
  out += append("fiber", formatFiberId(fiber3.id));
  const messages = ensure(message);
  for (let i = 0; i < messages.length; i++) {
    out += append("message", format(messages[i], {
      space
    }));
  }
  if (cause.reasons.length > 0) {
    out += append("cause", causePretty(cause));
  }
  const now = date.getTime();
  const spans = fiber3.getRef(CurrentLogSpans);
  for (const span2 of spans) {
    out += " " + formatLogSpan(span2, now);
  }
  const annotations = fiber3.getRef(CurrentLogAnnotations);
  for (const [label, value] of Object.entries(annotations)) {
    out += append(label, format(value, {
      space
    }));
  }
  return out;
};
var make5 = loggerMake;
var defaultLogger2 = defaultLogger;
var formatSimple = /* @__PURE__ */ loggerMake(/* @__PURE__ */ format2(escapeDoubleQuotes));
var formatLogFmt = /* @__PURE__ */ loggerMake(/* @__PURE__ */ format2(JSON.stringify, 0));
var formatStructured = /* @__PURE__ */ loggerMake(({
  cause,
  date,
  fiber: fiber3,
  logLevel,
  message
}) => {
  const annotationsObj = {};
  const spansObj = {};
  const annotations = fiber3.getRef(CurrentLogAnnotations);
  for (const [key, value] of Object.entries(annotations)) {
    annotationsObj[key] = structuredMessage(value);
  }
  const now = date.getTime();
  const spans = fiber3.getRef(CurrentLogSpans);
  for (const [label, timestamp] of spans) {
    spansObj[label] = now - timestamp;
  }
  const messageArr = ensure(message);
  return {
    message: messageArr.length === 1 ? structuredMessage(messageArr[0]) : messageArr.map(structuredMessage),
    level: logLevel.toUpperCase(),
    timestamp: date.toISOString(),
    cause: cause.reasons.length > 0 ? causePretty(cause) : void 0,
    annotations: annotationsObj,
    spans: spansObj,
    fiberId: formatFiberId(fiber3.id)
  };
});
var formatJson2 = /* @__PURE__ */ map3(formatStructured, formatJson);
var batched = /* @__PURE__ */ dual(2, (self, options) => flatMap(scope, (scope3) => {
  let buffer = [];
  const flush = suspend(() => {
    if (buffer.length === 0) {
      return void_;
    }
    const arr = buffer;
    buffer = [];
    return options.flush(arr);
  });
  return uninterruptibleMask((restore) => restore(sleep(options.window).pipe(andThen(flush), forever)).pipe(forkDetach, flatMap((fiber3) => scopeAddFinalizerExit(scope3, () => fiberInterrupt(fiber3))), andThen(addFinalizer(() => flush)), as(loggerMake((options2) => {
    buffer.push(self.log(options2));
  }))));
}));
var consolePretty2 = consolePretty;
var consoleLogFmt = /* @__PURE__ */ withConsoleLog(formatLogFmt);
var consoleStructured = /* @__PURE__ */ withConsoleLog(formatStructured);
var consoleJson = /* @__PURE__ */ withConsoleLog(formatJson2);
var tracerLogger2 = tracerLogger;
var layer = (loggers, options) => effectServices(withFiber(fnUntraced(function* (fiber3) {
  const currentLoggers = new Set(options?.mergeWithExisting === true ? fiber3.getRef(CurrentLoggers) : []);
  for (const logger of loggers) {
    currentLoggers.add(isEffect(logger) ? yield* logger : logger);
  }
  return make3(CurrentLoggers, currentLoggers);
})));
var toFile = /* @__PURE__ */ dual((args3) => isLogger(args3[0]), (self, path, options) => gen(function* () {
  const fs = yield* FileSystem;
  const logFile2 = yield* fs.open(path, {
    flag: "a+",
    ...options
  });
  const encoder = new TextEncoder();
  return yield* batched(self, {
    window: options?.batchWindow ?? 1e3,
    flush: (output) => ignore(logFile2.write(encoder.encode(output.join("\n") + "\n")))
  });
}));

// src/index.ts
import { join as join7 } from "path";

// src/log.ts
import { appendFileSync, existsSync, mkdirSync, statSync, renameSync, unlinkSync } from "fs";
import { dirname, join } from "path";
var LOG_LEVELS = { debug: 0, info: 1, warn: 2, error: 3 };
var currentLogLevel = process.env.EMACS_BRIDGE_LOG_LEVEL || "warn";
var logFile = process.env.EMACS_BRIDGE_LOG_FILE || "/tmp/emacs-bridge.log";
var MAX_SIZE = parseInt(process.env.EMACS_BRIDGE_LOG_MAX_SIZE || "1048576", 10);
var rotationChecked = false;
function initLogForSession(transcriptPath) {
  if (!transcriptPath) return;
  const transcriptDir = dirname(transcriptPath);
  const gravityDir = join(transcriptDir, "gravity");
  if (!existsSync(gravityDir)) {
    mkdirSync(gravityDir, { recursive: true });
  }
  logFile = join(gravityDir, "bridge.log");
}
function rotateIfNeeded(path, maxSize) {
  try {
    const stats = statSync(path);
    if (stats.size > maxSize) {
      const backup = path + ".1";
      try {
        unlinkSync(backup);
      } catch {
      }
      renameSync(path, backup);
    }
  } catch {
  }
}
function log2(msg, level = "debug") {
  if (LOG_LEVELS[level] < LOG_LEVELS[currentLogLevel]) return;
  try {
    if (!rotationChecked) {
      rotationChecked = true;
      rotateIfNeeded(logFile, MAX_SIZE);
    }
    const timestamp = (/* @__PURE__ */ new Date()).toISOString();
    appendFileSync(logFile, `[${timestamp}] [${level.toUpperCase()}] ${msg}
`);
  } catch {
  }
}

// src/dump.ts
import { join as join2, dirname as dirname2 } from "path";
import { existsSync as existsSync2, mkdirSync as mkdirSync2, readFileSync, writeFileSync } from "fs";
function getDumpDir(transcriptPath) {
  if (!transcriptPath) return void 0;
  const transcriptDir = dirname2(transcriptPath);
  return join2(transcriptDir, "gravity", "dumps");
}
function nextDumpSeq(transcriptPath) {
  const dumpDir = getDumpDir(transcriptPath);
  if (!dumpDir) return void 0;
  if (!existsSync2(dumpDir)) mkdirSync2(dumpDir, { recursive: true });
  const counterFile = join2(dumpDir, "_counter.txt");
  let counter = 0;
  try {
    counter = parseInt(readFileSync(counterFile, "utf-8").trim(), 10) || 0;
  } catch {
  }
  counter++;
  writeFileSync(counterFile, String(counter));
  return counter;
}
function writeDumpFile(transcriptPath, seq, eventName, suffix, data) {
  if (!transcriptPath || seq === void 0) return;
  const dumpDir = getDumpDir(transcriptPath);
  if (!dumpDir) return;
  try {
    if (!existsSync2(dumpDir)) mkdirSync2(dumpDir, { recursive: true });
    const filename = `${String(seq).padStart(4, "0")}__${eventName}__${suffix}.json`;
    writeFileSync(join2(dumpDir, filename), JSON.stringify(data, null, 2) + "\n");
  } catch (e) {
    log2(`writeDumpFile error: ${e}`, "error");
  }
}

// src/enrich.ts
import { existsSync as existsSync5, statSync as statSync3 } from "fs";

// src/enrichment.ts
import { existsSync as existsSync4, readFileSync as readFileSync3, statSync as statSync2, openSync, readSync, closeSync } from "fs";

// src/agent-state.ts
import { join as join3, dirname as dirname3, basename } from "path";
import { existsSync as existsSync3, readFileSync as readFileSync2, writeFileSync as writeFileSync2, mkdirSync as mkdirSync3 } from "fs";
function getAgentStatePath(transcriptPath) {
  if (!transcriptPath) return void 0;
  const transcriptDir = dirname3(transcriptPath);
  return join3(transcriptDir, "gravity", "emacs-bridge-agents.json");
}
function readAgentState(cwd, transcriptPath) {
  const statePath = transcriptPath ? getAgentStatePath(transcriptPath) : void 0;
  if (!statePath) return {};
  try {
    if (existsSync3(statePath)) {
      return JSON.parse(readFileSync2(statePath, "utf-8"));
    }
  } catch (e) {
    log2(`readAgentState error: ${e}`, "error");
  }
  return {};
}
function writeAgentState(cwd, transcriptPath, state) {
  const statePath = transcriptPath ? getAgentStatePath(transcriptPath) : void 0;
  if (!statePath) return;
  try {
    const dir = dirname3(statePath);
    if (!existsSync3(dir)) mkdirSync3(dir, { recursive: true });
    writeFileSync2(statePath, JSON.stringify(state), "utf-8");
  } catch (e) {
    log2(`writeAgentState error: ${e}`, "error");
  }
}
function agentTranscriptPath(transcriptPath, sessionId, agentId) {
  const transcriptDir = dirname3(transcriptPath);
  const sessionBase = basename(transcriptPath, ".jsonl");
  return join3(transcriptDir, sessionBase, "subagents", `agent-${agentId}.jsonl`);
}
function transcriptHasToolUseId(agentTranscript, toolUseId) {
  try {
    if (!existsSync3(agentTranscript)) return false;
    const content = readTail(agentTranscript, 5 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c)) continue;
        for (const block of c) {
          if (block.type === "tool_use" && block.id === toolUseId) return true;
        }
      } catch {
        continue;
      }
    }
  } catch (e) {
    log2(`transcriptHasToolUseId error: ${e}`, "error");
  }
  return false;
}
function extractAgentToolIds(agentTranscript) {
  const ids = [];
  try {
    if (!existsSync3(agentTranscript)) return ids;
    const content = readFileSync2(agentTranscript, "utf-8");
    const lines = content.split("\n").filter((l) => l.length > 0);
    for (const line of lines) {
      try {
        const obj = JSON.parse(line);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c)) continue;
        for (const block of c) {
          if (block.type === "tool_use" && block.id) ids.push(block.id);
        }
      } catch {
        continue;
      }
    }
  } catch (e) {
    log2(`extractAgentToolIds error: ${e}`, "error");
  }
  return ids;
}
function attributeToolToAgent(sessionId, cwd, transcriptPath, toolUseId, activeAgents) {
  if (activeAgents.length === 0) return { parentAgentId: null };
  if (activeAgents.length === 1) return { parentAgentId: activeAgents[0] };
  if (transcriptPath && toolUseId) {
    for (const agentId of activeAgents) {
      const atp = agentTranscriptPath(transcriptPath, sessionId, agentId);
      if (transcriptHasToolUseId(atp, toolUseId)) {
        log2(`Attributed tool ${toolUseId} to agent ${agentId} via transcript lookup`);
        return { parentAgentId: agentId };
      }
    }
  }
  log2(`Tool ${toolUseId} ambiguous among ${activeAgents.length} agents`, "warn");
  return { parentAgentId: "ambiguous", candidateAgentIds: [...activeAgents] };
}

// src/enrichment.ts
function readTail(filePath, maxBytes) {
  const stat = statSync2(filePath);
  const size = stat.size;
  if (size <= maxBytes) {
    return readFileSync3(filePath, "utf-8");
  }
  const fd = openSync(filePath, "r");
  const buffer = Buffer.alloc(maxBytes);
  readSync(fd, buffer, 0, maxBytes, size - maxBytes);
  closeSync(fd);
  const text = buffer.toString("utf-8");
  const firstNewline = text.indexOf("\n");
  return firstNewline >= 0 ? text.substring(firstNewline + 1) : text;
}
function readHead(filePath, maxBytes) {
  const stat = statSync2(filePath);
  const size = stat.size;
  if (size <= maxBytes) {
    return readFileSync3(filePath, "utf-8");
  }
  const fd = openSync(filePath, "r");
  const buffer = Buffer.alloc(maxBytes);
  readSync(fd, buffer, 0, maxBytes, 0);
  closeSync(fd);
  const text = buffer.toString("utf-8");
  const lastNewline = text.lastIndexOf("\n");
  return lastNewline >= 0 ? text.substring(0, lastNewline) : text;
}
function extractPrecedingContent(transcriptPath, toolUseId) {
  const result3 = { text: "", thinking: "", model: "" };
  try {
    const content = readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);
    let startIdx = -1;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        if (c[0].type === "tool_use" && c[0].id === toolUseId) {
          startIdx = i;
          if (obj.message?.model) result3.model = obj.message.model;
          break;
        }
      } catch {
        continue;
      }
    }
    if (startIdx < 0) {
      startIdx = lines.length;
    }
    const textParts = [];
    for (let i = startIdx - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") break;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        const blockType = c[0].type;
        if (!result3.model && obj.message?.model) result3.model = obj.message.model;
        if (blockType === "tool_use") continue;
        if (blockType === "tool_result") break;
        if (blockType === "text") {
          const text = c[0].text || "";
          if (text && text !== "(no content)") {
            textParts.unshift(text);
          }
          continue;
        }
        if (blockType === "thinking" && !result3.thinking) {
          result3.thinking = c[0].thinking || "";
          break;
        }
        break;
      } catch {
        continue;
      }
    }
    result3.text = textParts.join("\n\n");
    return result3;
  } catch (e) {
    log2(`extractPrecedingContent error: ${e}`, "error");
    return result3;
  }
}
function extractTokenUsage(transcriptPath) {
  const result3 = { input_tokens: 0, output_tokens: 0, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 };
  try {
    const content = readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);
    for (const line of lines) {
      try {
        const obj = JSON.parse(line);
        const usage = obj.message?.usage ?? obj.usage;
        if (usage) {
          result3.input_tokens += usage.input_tokens || 0;
          result3.output_tokens += usage.output_tokens || 0;
          result3.cache_read_input_tokens += usage.cache_read_input_tokens || 0;
          result3.cache_creation_input_tokens += usage.cache_creation_input_tokens || 0;
        }
      } catch {
        continue;
      }
    }
    log2(`extractTokenUsage: in=${result3.input_tokens} out=${result3.output_tokens} cache_read=${result3.cache_read_input_tokens} cache_create=${result3.cache_creation_input_tokens}`, "info");
  } catch (e) {
    log2(`extractTokenUsage error: ${e}`, "error");
  }
  return result3;
}
function extractFollowingContent(transcriptPath, toolUseId) {
  const result3 = { text: "", thinking: "" };
  try {
    const content = readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);
    let toolResultIdx = -1;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "user" && obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c)) continue;
        for (const block of c) {
          if (block.type === "tool_result" && block.tool_use_id === toolUseId) {
            toolResultIdx = i;
            break;
          }
        }
        if (toolResultIdx >= 0) break;
      } catch {
        continue;
      }
    }
    if (toolResultIdx < 0) return result3;
    const textParts = [];
    for (let i = toolResultIdx + 1; i < lines.length; i++) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") break;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        const blockType = c[0].type;
        if (blockType === "text") {
          const text = c[0].text || "";
          if (text && text !== "(no content)") {
            textParts.push(text);
          }
          continue;
        }
        if (blockType === "thinking") {
          if (!result3.thinking) {
            result3.thinking = c[0].thinking || "";
          }
          continue;
        }
        break;
      } catch {
        continue;
      }
    }
    result3.text = textParts.join("\n\n");
    return result3;
  } catch (e) {
    log2(`extractFollowingContent error: ${e}`, "error");
    return result3;
  }
}
function extractTrailingTextFromAgent(lines) {
  const result3 = { text: "", thinking: "" };
  try {
    let foundThinking = false;
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant") continue;
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        for (const block of c) {
          if (block.type === "text") {
            const text = block.text || "";
            if (text && text !== "(no content)") {
              if (!result3.text) result3.text = text;
            }
          } else if (block.type === "thinking" && !foundThinking) {
            result3.thinking = block.thinking || "";
            foundThinking = true;
          }
        }
        if (result3.text || result3.thinking) {
          log2(`extractTrailingTextFromAgent: found text=${!!result3.text} (${result3.text.length} chars), thinking=${!!result3.thinking} (${result3.thinking.length} chars)`);
          return result3;
        }
      } catch {
        continue;
      }
    }
    log2(`extractTrailingTextFromAgent: no text/thinking found in ${lines.length} lines`);
    return result3;
  } catch (e) {
    log2(`extractTrailingTextFromAgent error: ${e}`, "error");
    return result3;
  }
}
function extractTrailingText(transcriptPath, maxBytes) {
  const result3 = { text: "", thinking: "" };
  try {
    if (!existsSync4(transcriptPath)) {
      log2(`extractTrailingText: file not found: ${transcriptPath}`, "warn");
      return result3;
    }
    const content = maxBytes ? readHead(transcriptPath, maxBytes) : readTail(transcriptPath, 2 * 1024 * 1024);
    const lines = content.split("\n").filter((l) => l.length > 0);
    let isSidechain = false;
    for (const line of lines.slice(0, 10)) {
      try {
        const obj = JSON.parse(line);
        if (obj.isSidechain !== void 0) {
          isSidechain = obj.isSidechain === true;
          break;
        }
      } catch {
      }
    }
    if (isSidechain) {
      log2(`extractTrailingText: detected sidechain format, using agent extraction`, "info");
      return extractTrailingTextFromAgent(lines);
    }
    const diagCount = Math.min(10, lines.length);
    const diagLines = [];
    for (let i = lines.length - diagCount; i < lines.length; i++) {
      try {
        const obj = JSON.parse(lines[i]);
        const t = obj.type || "?";
        const c = obj.message?.content;
        const block0 = Array.isArray(c) && c.length > 0 ? c[0].type : "-";
        const preview = block0 === "text" ? (c[0].text || "").substring(0, 60) : "";
        diagLines.push(`[${i}] ${t}/${block0} ${preview}`);
      } catch {
        diagLines.push(`[${i}] (parse error)`);
      }
    }
    log2(`extractTrailingText: ${lines.length} lines, tail:
  ${diagLines.join("\n  ")}`);
    const textParts = [];
    let stopReason = "exhausted";
    for (let i = lines.length - 1; i >= 0; i--) {
      try {
        const obj = JSON.parse(lines[i]);
        if (obj.type !== "assistant" && obj.type !== "user") continue;
        if (obj.type === "user") {
          const uc = obj.message?.content;
          if (Array.isArray(uc) && uc.some((b) => b.type === "text")) {
            stopReason = `user_text@${i}`;
            break;
          }
          continue;
        }
        const c = obj.message?.content;
        if (!Array.isArray(c) || c.length === 0) continue;
        let hasThinking = false;
        let hasToolUse = false;
        for (const block of c) {
          if (block.type === "text") {
            const text = block.text || "";
            if (text && text !== "(no content)") {
              textParts.unshift(text);
            }
          } else if (block.type === "thinking") {
            result3.thinking = block.thinking || "";
            hasThinking = true;
          } else {
            stopReason = `${block.type}@${i}`;
            hasToolUse = true;
          }
        }
        if (hasThinking || hasToolUse) break;
      } catch {
        continue;
      }
    }
    result3.text = textParts.join("\n\n");
    log2(`extractTrailingText result: ${result3.text.length} chars text, ${result3.thinking.length} chars thinking, stop=${stopReason}, parts=${textParts.length}`);
    return result3;
  } catch (e) {
    log2(`extractTrailingText error: ${e}`, "error");
    return result3;
  }
}
function extractTranscriptMeta(transcriptPath) {
  const result3 = { slug: null, gitBranch: null };
  try {
    const fd = openSync(transcriptPath, "r");
    const buffer = Buffer.alloc(64 * 1024);
    const bytesRead = readSync(fd, buffer, 0, buffer.length, 0);
    closeSync(fd);
    if (bytesRead === 0) return result3;
    const text = buffer.toString("utf-8", 0, bytesRead);
    const lines = text.split("\n");
    for (const line of lines) {
      if (!line.length) continue;
      try {
        const obj = JSON.parse(line);
        if (!result3.slug && obj.slug) result3.slug = obj.slug;
        if (!result3.gitBranch && obj.gitBranch) result3.gitBranch = obj.gitBranch;
        if (result3.slug && result3.gitBranch) break;
      } catch {
        continue;
      }
    }
  } catch {
  }
  return result3;
}
function extractSlug(transcriptPath) {
  return extractTranscriptMeta(transcriptPath).slug;
}

// src/enrich.ts
function enrichSessionMetadata(inputData, transcriptPath) {
  if (!transcriptPath) return inputData;
  const meta = extractTranscriptMeta(transcriptPath);
  if (meta.slug) {
    log2(`Extracted slug: ${meta.slug}`);
  }
  return {
    ...inputData,
    ...meta.slug && { slug: meta.slug },
    ...meta.gitBranch && { branch: meta.gitBranch }
  };
}
function enrichSubagentStart(inputData, sessionId, cwd, transcriptPath) {
  const agentId = inputData.agent_id;
  if (!agentId || !cwd) return inputData;
  const state = readAgentState(cwd, transcriptPath);
  if (!state[sessionId]) state[sessionId] = [];
  if (!state[sessionId].includes(agentId)) {
    state[sessionId].push(agentId);
  }
  writeAgentState(cwd, transcriptPath, state);
  log2(`Agent ${agentId} started, active list: ${state[sessionId].join(", ")}`, "info");
  if (!transcriptPath) return inputData;
  return {
    ...inputData,
    agent_transcript_path: agentTranscriptPath(transcriptPath, sessionId, agentId)
  };
}
function enrichSubagentStop(inputData, sessionId, cwd, transcriptPath) {
  log2(`[SUBAGENT_STOP_ENTERED] event processing started`, "warn");
  const agentId = inputData.agent_id;
  if (!agentId || !cwd) return inputData;
  const state = readAgentState(cwd, transcriptPath);
  if (state[sessionId]) {
    state[sessionId] = state[sessionId].filter((id) => id !== agentId);
    if (state[sessionId].length === 0) delete state[sessionId];
  }
  writeAgentState(cwd, transcriptPath, state);
  log2(`Agent ${agentId} stopped, active list: ${state[sessionId]?.join(", ") || "(empty)"}`, "warn");
  const providedAtp = inputData.agent_transcript_path;
  log2(`SubagentStop: agent_id=${agentId}, has_atp=${!!providedAtp}`, "warn");
  if (!providedAtp) {
    log2(`SubagentStop: Claude Code did not provide agent_transcript_path`, "warn");
    return inputData;
  }
  const atpExists = existsSync5(providedAtp);
  log2(`SubagentStop: atp_exists=${atpExists}, path=${providedAtp}`, "warn");
  const toolIds = extractAgentToolIds(providedAtp);
  if (toolIds.length > 0) {
    log2(`Agent ${agentId} had ${toolIds.length} tool calls`, "warn");
  }
  let text = "";
  let thinking = "";
  try {
    const extracted = extractTrailingText(providedAtp);
    text = extracted.text;
    thinking = extracted.thinking;
    log2(`SubagentStop: initial extraction: text=${text.length}B, thinking=${thinking.length}B`, "warn");
    if (!text && !thinking) {
      log2(`SubagentStop: no text/thinking in agent transcript, trying main transcript fallback`, "warn");
      try {
        if (transcriptPath) {
          const fallback = extractTrailingText(transcriptPath);
          text = fallback.text;
          thinking = fallback.thinking;
        }
        if (text || thinking) {
          log2(`SubagentStop: extracted from main transcript: text=${text.length}B, thinking=${thinking.length}B`, "warn");
        }
      } catch (e) {
        log2(`SubagentStop: main transcript fallback failed: ${e}`, "warn");
      }
    }
    if (!text && !thinking) {
      log2(`SubagentStop: FAILED to extract any text/thinking for agent ${agentId}`, "warn");
    }
  } catch (e) {
    log2(`Failed to extract agent trailing content: ${e}`, "error");
  }
  return {
    ...inputData,
    ...toolIds.length > 0 && { agent_tool_ids: toolIds },
    ...text && { agent_stop_text: text },
    ...thinking && { agent_stop_thinking: thinking }
  };
}
function enrichSessionEnd(inputData, sessionId, cwd, transcriptPath) {
  if (!cwd) return inputData;
  const state = readAgentState(cwd, transcriptPath);
  if (state[sessionId]) {
    delete state[sessionId];
    writeAgentState(cwd, transcriptPath, state);
    log2(`Cleaned up agent state for session ${sessionId}`, "info");
  }
  return inputData;
}
function enrichToolAttribution(inputData, sessionId, cwd, transcriptPath) {
  if (!cwd) return inputData;
  const state = readAgentState(cwd, transcriptPath);
  const activeAgents = state[sessionId] || [];
  if (activeAgents.length === 0) return inputData;
  const toolUseId = inputData.tool_use_id || "";
  const { parentAgentId, candidateAgentIds } = attributeToolToAgent(
    sessionId,
    cwd,
    transcriptPath,
    toolUseId,
    activeAgents
  );
  if (!parentAgentId) return inputData;
  log2(`Tool ${toolUseId} attributed to agent: ${parentAgentId}`);
  return {
    ...inputData,
    parent_agent_id: parentAgentId,
    ...candidateAgentIds && { candidate_agent_ids: candidateAgentIds }
  };
}
function enrichPreToolUse(inputData, sessionId, transcriptPath) {
  const toolUseId = inputData.tool_use_id;
  const parentAgentId = inputData.parent_agent_id;
  const effectiveTranscript = parentAgentId && parentAgentId !== "ambiguous" && transcriptPath ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId) : transcriptPath;
  if (!effectiveTranscript || !toolUseId) return inputData;
  let text = "";
  let thinking = "";
  let model;
  try {
    const extracted = extractPrecedingContent(effectiveTranscript, toolUseId);
    text = extracted.text;
    thinking = extracted.thinking;
    model = extracted.model;
    if (text) {
      log2(`Extracted assistant text (${text.length} chars) for ${toolUseId}`);
    }
    if (thinking) {
      log2(`Extracted thinking (${thinking.length} chars) for ${toolUseId}`);
    }
  } catch (e) {
    log2(`Failed to extract preceding content: ${e}`, "error");
  }
  const toolName = inputData.tool_name;
  const toolInput = inputData.tool_input;
  const requestedModel = toolName === "Task" && toolInput?.model ? String(toolInput.model) : void 0;
  return {
    ...inputData,
    ...text && { assistant_text: text },
    ...thinking && { assistant_thinking: thinking },
    ...model && { model },
    ...requestedModel && { requested_model: requestedModel }
  };
}
function enrichPostToolUse(inputData, sessionId, transcriptPath) {
  const toolUseId = inputData.tool_use_id;
  const parentAgentId = inputData.parent_agent_id;
  const effectiveTranscript = parentAgentId && parentAgentId !== "ambiguous" && transcriptPath ? agentTranscriptPath(transcriptPath, sessionId, parentAgentId) : transcriptPath;
  if (!effectiveTranscript || !toolUseId) return inputData;
  let text = "";
  let thinking = "";
  try {
    const extracted = extractFollowingContent(effectiveTranscript, toolUseId);
    text = extracted.text;
    thinking = extracted.thinking;
    if (text) {
      log2(`Extracted post-tool text (${text.length} chars) for ${toolUseId}`);
    }
    if (thinking) {
      log2(`Extracted post-tool thinking (${thinking.length} chars) for ${toolUseId}`);
    }
  } catch (e) {
    log2(`Failed to extract following content: ${e}`, "error");
  }
  return {
    ...inputData,
    ...text && { post_tool_text: text },
    ...thinking && { post_tool_thinking: thinking }
  };
}
async function enrichStop(inputData, transcriptPath) {
  if (!transcriptPath) return inputData;
  let text = "";
  let thinking = "";
  try {
    let snapshotBytes;
    try {
      snapshotBytes = statSync3(transcriptPath).size;
      log2(`Stop: transcript snapshot ${snapshotBytes} bytes`, "warn");
    } catch {
    }
    const extracted = extractTrailingText(transcriptPath, snapshotBytes);
    text = extracted.text;
    thinking = extracted.thinking;
    const maxRetries = 3;
    const delayMs = 150;
    for (let retry3 = 0; retry3 < maxRetries; retry3++) {
      await new Promise((r) => setTimeout(r, delayMs));
      let newSize;
      try {
        newSize = statSync3(transcriptPath).size;
      } catch {
      }
      if (newSize && newSize > (snapshotBytes ?? 0)) {
        snapshotBytes = newSize;
        const retryExtracted = extractTrailingText(transcriptPath, snapshotBytes);
        if (retryExtracted.text) text = retryExtracted.text;
        if (retryExtracted.thinking) thinking = retryExtracted.thinking;
        log2(`Stop retry ${retry3 + 1}: file grew to ${newSize}, ${text.length} chars text, ${thinking.length} chars thinking`, "warn");
      } else {
        break;
      }
    }
    if (!text) {
      log2(`Stop: no trailing text found`, "warn");
    }
  } catch (e) {
    log2(`Failed to extract trailing content: ${e}`, "error");
  }
  let tokenUsage;
  try {
    tokenUsage = extractTokenUsage(transcriptPath);
  } catch (e) {
    log2(`Failed to extract token usage: ${e}`, "error");
  }
  return {
    ...inputData,
    ...text && { stop_text: text },
    ...thinking && { stop_thinking: thinking },
    ...tokenUsage && { token_usage: tokenUsage }
  };
}

// src/services/process-io.ts
import { readFileSync as readFileSync4 } from "fs";

// src/services/errors.ts
var StdinReadError = class extends Data_exports.TaggedError("StdinReadError") {
};
var StdinParseError = class extends Data_exports.TaggedError("StdinParseError") {
};
var SocketNotFoundError = class extends Data_exports.TaggedError("SocketNotFoundError") {
};
var SocketError = class extends Data_exports.TaggedError("SocketError") {
};
var SocketTimeoutError = class extends Data_exports.TaggedError("SocketTimeoutError") {
};

// src/services/process-io.ts
var ProcessIO = ServiceMap_exports.Service("ProcessIO");
var ProcessIOLive = Layer_exports.succeed(ProcessIO, {
  readStdin: () => Effect_exports.try({
    try: () => {
      const buf = readFileSync4(0);
      return buf.length > 0 ? buf.toString() : "";
    },
    catch: (cause) => new StdinReadError({ cause })
  }),
  writeStdout: (data) => Effect_exports.sync(() => {
    process.stdout.write(data);
  }),
  writeStdoutRaw: (data) => Effect_exports.sync(() => {
    process.stdout.write(data);
  }),
  getArg: (index) => Effect_exports.sync(() => process.argv[index]),
  getEnv: (key) => Effect_exports.sync(() => process.env[key]),
  exit: (code) => Effect_exports.sync(() => {
    process.exit(code);
  })
});

// src/services/config.ts
import { execSync } from "child_process";

// ../shared/src/safe-bash.ts
import { basename as basename2 } from "path";

// ../shared/src/services/errors.ts
var FileReadError = class extends Data_exports.TaggedError("FileReadError") {
};
var FileWriteError = class extends Data_exports.TaggedError("FileWriteError") {
};

// ../shared/src/services/fs.ts
import {
  readFileSync as readFileSync5,
  writeFileSync as writeFileSync3,
  appendFileSync as appendFileSync2,
  existsSync as existsSync6,
  statSync as statSync4,
  openSync as openSync2,
  readSync as readSync2,
  closeSync as closeSync2,
  mkdirSync as mkdirSync4,
  unlinkSync as unlinkSync2,
  renameSync as renameSync2
} from "fs";
var Fs = ServiceMap_exports.Service("Fs");
var FsLive = Layer_exports.succeed(Fs, {
  readFile: (path) => Effect_exports.try({
    try: () => readFileSync5(path, "utf-8"),
    catch: (cause) => new FileReadError({ path, cause })
  }),
  writeFile: (path, data) => Effect_exports.try({
    try: () => {
      writeFileSync3(path, data, "utf-8");
    },
    catch: (cause) => new FileWriteError({ path, cause })
  }),
  appendFile: (path, data) => Effect_exports.try({
    try: () => {
      appendFileSync2(path, data, "utf-8");
    },
    catch: (cause) => new FileWriteError({ path, cause })
  }),
  exists: (path) => Effect_exports.sync(() => existsSync6(path)),
  stat: (path) => Effect_exports.try({
    try: () => {
      const s = statSync4(path);
      return { size: s.size };
    },
    catch: (cause) => new FileReadError({ path, cause })
  }),
  readBytes: (path, offset, length) => Effect_exports.try({
    try: () => {
      const fd = openSync2(path, "r");
      const buf = Buffer.alloc(length);
      readSync2(fd, buf, 0, length, offset);
      closeSync2(fd);
      return buf;
    },
    catch: (cause) => new FileReadError({ path, cause })
  }),
  mkdirp: (path) => Effect_exports.try({
    try: () => {
      mkdirSync4(path, { recursive: true });
    },
    catch: (cause) => new FileWriteError({ path, cause })
  }),
  unlinkIfExists: (path) => Effect_exports.sync(() => {
    try {
      unlinkSync2(path);
    } catch {
    }
  }),
  rename: (from, to) => Effect_exports.try({
    try: () => {
      renameSync2(from, to);
    },
    catch: (cause) => new FileWriteError({ path: from, cause })
  })
});

// src/services/config.ts
import { join as join4 } from "path";
var BridgeConfig = ServiceMap_exports.Service("BridgeConfig");
var BridgeConfigLive = Layer_exports.effect(
  BridgeConfig,
  Effect_exports.gen(function* () {
    const io = yield* Effect_exports.service(ProcessIO);
    const fs = yield* Effect_exports.service(Fs);
    const gravitySock = yield* io.getEnv("CLAUDE_GRAVITY_SOCK");
    const sockDir = yield* io.getEnv("CLAUDE_GRAVITY_SOCK_DIR");
    const home = (yield* io.getEnv("HOME")) ?? "/tmp";
    const socketPath = gravitySock ?? (sockDir ? join4(sockDir, "claude-gravity.sock") : join4(home, ".local", "state", "claude-gravity.sock"));
    const dumpDir = yield* io.getEnv("CLAUDE_GRAVITY_DUMP_DIR");
    const dumpEnabled = !!dumpDir || (yield* io.getEnv("CLAUDE_GRAVITY_DUMP")) === "1";
    const noAutoApprove = (yield* io.getEnv("CLAUDE_GRAVITY_NO_AUTO_APPROVE")) === "1";
    const claudePidStr = yield* io.getEnv("CLAUDE_PID");
    const claudePid = claudePidStr ? parseInt(claudePidStr, 10) || null : null;
    const tempId = (yield* io.getEnv("CLAUDE_GRAVITY_TEMP_ID")) ?? null;
    let tmuxSession = null;
    const tmuxEnv = yield* io.getEnv("TMUX");
    if (tmuxEnv) {
      try {
        tmuxSession = execSync(
          'tmux display-message -p "#{session_name}"',
          { encoding: "utf-8", timeout: 1e3 }
        ).trim() || null;
      } catch {
      }
    }
    let effortLevel = null;
    const settingsPath = join4(home, ".claude", "settings.json");
    const settingsExists = yield* fs.exists(settingsPath);
    if (settingsExists) {
      const content = yield* fs.readFile(settingsPath).pipe(
        Effect_exports.catch(() => Effect_exports.succeed(""))
      );
      if (content) {
        try {
          const settings = JSON.parse(content);
          effortLevel = settings.effortLevel ?? null;
        } catch {
        }
      }
    }
    return {
      socketPath,
      dumpDir,
      dumpEnabled,
      noAutoApprove,
      claudePid,
      tempId,
      tmuxSession,
      effortLevel
    };
  })
);

// src/services/logger.ts
import { appendFileSync as appendFileSync3 } from "fs";
var LOG_FILE = "/tmp/emacs-bridge.log";
var FileLogger = Logger_exports.make(({ message, logLevel }) => {
  try {
    const timestamp = (/* @__PURE__ */ new Date()).toISOString();
    const level = typeof logLevel === "string" ? logLevel.toUpperCase() : "LOG";
    const text = Array.isArray(message) ? message.map((m) => typeof m === "string" ? m : JSON.stringify(m)).join(" ") : typeof message === "string" ? message : JSON.stringify(message);
    appendFileSync3(LOG_FILE, `[${timestamp}] [${level}] ${text}
`);
  } catch {
  }
});
var LoggerLive = Logger_exports.layer([FileLogger]);

// src/services/emacs-socket.ts
import { createConnection } from "net";
import { existsSync as existsSync7 } from "fs";
var EmacsSocket = ServiceMap_exports.Service("EmacsSocket");
function makeEmacsSocket(socketPath) {
  return {
    send: (payload) => Effect_exports.callback((resume) => {
      const client = createConnection(socketPath);
      client.on("connect", () => {
        const message = JSON.stringify(payload) + "\n";
        const flushed = client.write(message);
        if (flushed) {
          client.end();
        } else {
          client.once("drain", () => client.end());
        }
      });
      client.on("error", (_err) => {
        resume(Effect_exports.void);
      });
      client.on("close", () => {
        resume(Effect_exports.void);
      });
    }),
    sendAndWait: (payload, timeoutMs = 3456e5) => Effect_exports.callback((resume) => {
      const client = createConnection(socketPath);
      let responded = false;
      let buffer = "";
      const timer = setTimeout(() => {
        if (!responded) {
          responded = true;
          client.destroy();
          resume(Effect_exports.succeed({}));
        }
      }, timeoutMs);
      client.on("connect", () => {
        const p = { ...payload, needs_response: true };
        const message = JSON.stringify(p) + "\n";
        client.write(message);
      });
      client.on("data", (chunk) => {
        buffer += chunk.toString();
        const newlineIdx = buffer.indexOf("\n");
        if (newlineIdx >= 0 && !responded) {
          responded = true;
          clearTimeout(timer);
          const line = buffer.substring(0, newlineIdx);
          try {
            resume(Effect_exports.succeed(JSON.parse(line)));
          } catch {
            resume(Effect_exports.succeed({}));
          }
          client.destroy();
        }
      });
      client.on("error", (_err) => {
        if (!responded) {
          responded = true;
          clearTimeout(timer);
          resume(Effect_exports.succeed({}));
        }
      });
      client.on("close", () => {
        if (!responded) {
          responded = true;
          clearTimeout(timer);
          resume(Effect_exports.succeed({}));
        }
      });
    }),
    socketExists: () => Effect_exports.sync(() => existsSync7(socketPath))
  };
}
var EmacsSocketLive = (socketPath) => Layer_exports.succeed(EmacsSocket, makeEmacsSocket(socketPath));

// src/services/hook-socket.ts
import { createConnection as createConnection2 } from "net";
import { existsSync as existsSync8 } from "fs";
import { join as join5 } from "path";
var HookSocketClient = ServiceMap_exports.Service("HookSocketClient");
function makeHookSocketClient(socketPath) {
  return {
    send: (msg) => Effect_exports.callback((resume) => {
      if (!existsSync8(socketPath)) {
        resume(Effect_exports.void);
        return;
      }
      const client = createConnection2(socketPath);
      let settled = false;
      const timer = setTimeout(() => {
        if (!settled) {
          settled = true;
          client.destroy();
          resume(Effect_exports.void);
        }
      }, 2e3);
      timer.unref();
      client.on("connect", () => {
        const payload = JSON.stringify(msg) + "\n";
        const flushed = client.write(payload);
        if (flushed) {
          client.end();
        } else {
          client.once("drain", () => client.end());
        }
      });
      client.on("error", () => {
        if (!settled) {
          settled = true;
          clearTimeout(timer);
          resume(Effect_exports.void);
        }
      });
      client.on("close", () => {
        if (!settled) {
          settled = true;
          clearTimeout(timer);
          resume(Effect_exports.void);
        }
      });
    }),
    sendAndWait: (msg, timeoutMs = 3456e5) => Effect_exports.callback((resume) => {
      if (!existsSync8(socketPath)) {
        resume(Effect_exports.succeed({}));
        return;
      }
      const client = createConnection2(socketPath);
      let responded = false;
      let buffer = "";
      const timer = setTimeout(() => {
        if (!responded) {
          responded = true;
          client.destroy();
          resume(Effect_exports.succeed({}));
        }
      }, timeoutMs);
      timer.unref();
      client.on("connect", () => {
        const payload = JSON.stringify({ ...msg, needs_response: true }) + "\n";
        client.write(payload);
      });
      client.on("data", (chunk) => {
        buffer += chunk.toString();
        const newlineIdx = buffer.indexOf("\n");
        if (newlineIdx >= 0 && !responded) {
          responded = true;
          clearTimeout(timer);
          const line = buffer.substring(0, newlineIdx);
          try {
            resume(Effect_exports.succeed(JSON.parse(line)));
          } catch {
            resume(Effect_exports.succeed({}));
          }
          client.destroy();
        }
      });
      client.on("error", () => {
        if (!responded) {
          responded = true;
          clearTimeout(timer);
          resume(Effect_exports.succeed({}));
        }
      });
      client.on("close", () => {
        if (!responded) {
          responded = true;
          clearTimeout(timer);
          resume(Effect_exports.succeed({}));
        }
      });
    })
  };
}
function resolveHookSocketPath() {
  const fromEnv = process.env.GRAVITY_HOOK_SOCK;
  if (fromEnv) return fromEnv;
  const home = process.env.HOME || "/tmp";
  return join5(home, ".local", "state", "gravity-hooks.sock");
}
function hookSocketExists() {
  return existsSync8(resolveHookSocketPath());
}
var HookSocketClientLive = Layer_exports.succeed(
  HookSocketClient,
  makeHookSocketClient(resolveHookSocketPath())
);

// src/index.ts
var parseStdin = (raw) => pipe(
  Effect_exports.try({
    try: () => raw.length > 0 ? JSON.parse(raw) : {},
    catch: (cause) => new Error(String(cause))
  }),
  Effect_exports.catch(() => Effect_exports.succeed({}))
);
var program = Effect_exports.gen(function* () {
  const io = yield* Effect_exports.service(ProcessIO);
  const socket = yield* Effect_exports.service(EmacsSocket);
  const hookSocket = yield* Effect_exports.service(HookSocketClient);
  const config = yield* Effect_exports.service(BridgeConfig);
  yield* Effect_exports.logDebug(`Process started: ${process.argv.join(" ")}`);
  process.stdout.on("error", (err) => {
    log2(`stdout error: ${err.message}`, "error");
  });
  const eventName = (yield* io.getArg(2)) ?? "unknown";
  const raw = yield* io.readStdin().pipe(Effect_exports.catch(() => Effect_exports.succeed("")));
  let inputData = yield* parseStdin(raw);
  yield* Effect_exports.logDebug(`Payload: ${JSON.stringify(inputData)}`);
  const hookSocketPath = resolveHookSocketPath();
  if (!hookSocketExists()) {
    log2(`Hook socket not found at ${hookSocketPath}, passing through`, "warn");
    yield* io.writeStdout(JSON.stringify({}) + "\n");
    return;
  }
  const rawHookInput = JSON.parse(JSON.stringify(inputData));
  const sessionId = inputData.session_id || "unknown";
  const cwd = inputData.cwd || "";
  const pid = config.claudePid;
  if (config.tempId) inputData.temp_id = config.tempId;
  if (config.tmuxSession) inputData.tmux_session = config.tmuxSession;
  if (config.effortLevel) inputData.effort_level = config.effortLevel;
  const transcriptPath = inputData.transcript_path;
  initLogForSession(transcriptPath);
  let dumpSeq;
  if (config.dumpEnabled && transcriptPath) {
    dumpSeq = nextDumpSeq(transcriptPath);
    writeDumpFile(transcriptPath, dumpSeq, eventName, "raw", inputData);
  }
  let enrichedData = enrichSessionMetadata(inputData, transcriptPath);
  if (eventName === "SubagentStart") {
    enrichedData = enrichSubagentStart(enrichedData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "SubagentStop") {
    enrichedData = enrichSubagentStop(enrichedData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "SessionEnd") {
    enrichedData = enrichSessionEnd(enrichedData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "PreToolUse" || eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
    enrichedData = enrichToolAttribution(enrichedData, sessionId, cwd, transcriptPath);
  }
  if (eventName === "PreToolUse") {
    enrichedData = enrichPreToolUse(enrichedData, sessionId, transcriptPath);
  }
  if (eventName === "PostToolUse" || eventName === "PostToolUseFailure") {
    enrichedData = enrichPostToolUse(enrichedData, sessionId, transcriptPath);
  }
  if (eventName === "Stop") {
    enrichedData = yield* Effect_exports.promise(() => enrichStop(enrichedData, transcriptPath));
  }
  if (config.dumpEnabled && transcriptPath && dumpSeq !== void 0) {
    writeDumpFile(transcriptPath, dumpSeq, eventName, "output", {
      event: eventName,
      session_id: sessionId,
      cwd,
      pid,
      data: enrichedData
    });
  }
  const isBidirectional = eventName === "PermissionRequest" || eventName === "AskUserQuestionIntercept";
  const hookMsg = {
    event: eventName,
    session_id: sessionId,
    cwd,
    pid,
    source: "bridge",
    data: enrichedData,
    needs_response: isBidirectional
  };
  if (isBidirectional) {
    const toolName = enrichedData.tool_name || "unknown";
    yield* Effect_exports.logWarning(
      `${eventName}: waiting for gravity-server response [tool=${toolName}, session=${sessionId}]`
    );
    yield* socket.send({
      event: eventName,
      session_id: sessionId,
      cwd,
      pid,
      data: enrichedData,
      hook_input: rawHookInput
    }).pipe(Effect_exports.catch(() => Effect_exports.void));
    const response = yield* hookSocket.sendAndWait(hookMsg).pipe(
      Effect_exports.catch(() => {
        log2(`${eventName}: hook socket sendAndWait failed`, "error");
        return Effect_exports.succeed({});
      })
    );
    const reason = response?.reason;
    if (reason === "no_capable_terminal") {
      yield* Effect_exports.logInfo(
        `${eventName}: no capable terminal connected \u2014 falling through to TUI [tool=${toolName}, session=${sessionId}]`
      );
    } else if (!response || Object.keys(response).length === 0) {
      yield* Effect_exports.logError(
        `${eventName}: empty response from gravity-server [tool=${toolName}, session=${sessionId}] \u2014 writing {} to stdout`
      );
    }
    const responseStr = JSON.stringify(response) + "\n";
    const hardExit = setTimeout(() => {
      log2(`${eventName}: hard exit timeout [tool=${toolName}]`, "error");
      process.exit(1);
    }, 5e3);
    hardExit.unref();
    process.stdout.write(responseStr, () => {
      log2(`${eventName}: stdout write callback OK [tool=${toolName}]`, "warn");
      setTimeout(() => process.exit(0), 50);
    });
  } else {
    yield* hookSocket.send(hookMsg).pipe(
      Effect_exports.catch(() => Effect_exports.logDebug("Hook socket send failed (server unavailable)"))
    );
    yield* socket.send({
      event: eventName,
      session_id: sessionId,
      cwd,
      pid,
      data: enrichedData,
      hook_input: rawHookInput
    });
    let hookResponse = {};
    if (eventName === "SessionStart") {
      const shortId = sessionId.slice(0, 8);
      hookResponse.systemMessage = `emacs-gravity: connected (session ${shortId}, pid ${pid})`;
    }
    yield* io.writeStdout(JSON.stringify(hookResponse) + "\n");
  }
});
var safe = pipe(
  program,
  Effect_exports.catch(
    (error) => Effect_exports.gen(function* () {
      yield* Effect_exports.logError("Bridge error: " + String(error));
      const io = yield* Effect_exports.service(ProcessIO);
      yield* io.writeStdout(JSON.stringify({}) + "\n");
    })
  )
);
var MainLive = Layer_exports.mergeAll(
  ProcessIOLive,
  FsLive,
  LoggerLive
);
var ConfigLayer = Layer_exports.provide(BridgeConfigLive, MainLive);
var socketPathFromEnv = (() => {
  const gravitySock = process.env.CLAUDE_GRAVITY_SOCK;
  if (gravitySock) return gravitySock;
  const sockDir = process.env.CLAUDE_GRAVITY_SOCK_DIR;
  if (sockDir) return join7(sockDir, "claude-gravity.sock");
  const home = process.env.HOME || "/tmp";
  return join7(home, ".local", "state", "claude-gravity.sock");
})();
var FullLayer = Layer_exports.mergeAll(MainLive, ConfigLayer, EmacsSocketLive(socketPathFromEnv), HookSocketClientLive);
var args2 = process.argv.slice(2);
if (args2.includes("--mode") && args2[args2.indexOf("--mode") + 1] === "opencode") {
  Promise.resolve().then(() => (init_opencode_bridge(), opencode_bridge_exports)).then(() => {
    log2("OpenCode bridge started");
  }).catch((e) => {
    log2(`Failed to load opencode-bridge: ${e}`, "error");
    process.exit(1);
  });
} else {
  Effect_exports.runPromise(Effect_exports.provide(safe, FullLayer)).catch(() => {
    try {
      process.stdout.write(JSON.stringify({}) + "\n");
    } catch {
    }
  });
}
export {
  extractFollowingContent,
  extractPrecedingContent,
  extractSlug,
  extractTokenUsage,
  extractTrailingText,
  extractTranscriptMeta,
  readHead,
  readTail
};
