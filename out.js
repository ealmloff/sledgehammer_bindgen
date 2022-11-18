let m, p, lss, sp, d, t, c, s, sl, op, parent, into, namespace, child, text, value, id, name, new_id;
export function init(_d) {
    d = _d;
    c = new TextDecoder();
}
export function update_memory(_m) {
    m = _m;
}
export function run() {
    t = m.getUint8(d);
    if (t & 0x01) {
        p = m.getUint32(d + 1);
    }
    if (t & 0x02) {
        if (t & 0x04) {
            lss = m.getUint32(d + 5);
        }
        sl = m.getUint32(d + 5);
        if (t & 0x08) {
            sp = lss;
            s = "";
            endRounded = sp + ((sl / 4) | 0) * 4;
            while (sp < endRounded) {
                t = m.getUint32(sp);
                s += String.fromCharCode(t >> 24, (t & 0x00FF0000) >> 16, (t & 0x0000FF00) >> 8, (t & 0x000000FF));
                sp += 4;
            }
            switch (lss + sl - sp) {
                case 3:
                    t = m.getUint32(sp);
                    s += String.fromCharCode(t >> 24, (t & 0x00FF0000) >> 16, (t & 0x0000FF00) >> 8);
                    break;
                case 2:
                    t = m.getUint16(sp);
                    s += String.fromCharCode(t >> 8, t & 0xFF);
                    break;
                case 1:
                    s += String.fromCharCode(m.getUint8(sp));
                    break;
                case 0:
                    break;
            }
        } else {
            s = c.decode(new DataView(m.buffer, lss, sl));
        }
        sp = 0;
    }
    for (; ;) {
        op = this.view.getUint32(p, true);
        p += 4;
        if (exOp()) return;
        op >>>= 4;
        if (exOp()) return;
        op >>>= 4;
        if (exOp()) return;
        op >>>= 4;
        if (exOp()) return;
        op >>>= 4;
        if (exOp()) return;
        op >>>= 4;
        if (exOp()) return;
        op >>>= 4;
        if (exOp()) return;
        op >>>= 4;
        if (exOp()) return;
        op >>>= 4;
    }
}

function exOp() {
    switch (op & 15) {
        case 0:
            nodes = [];
            break;
        case 1:
            i = m.getUint32(p);
            p += 4;
            id, name = i & 1048575, s.substring(sp, sp += (i >>> 20) & 63);
            i = m.getUint32(p);
            p += 2;
            value = s.substring(sp, sp += i & 65535);
            nodes[id].setAttribute(name, value);
            break;
        case 2:
            i = m.getUint32(p);
            p += 4;
            id, namespace, name = i & 1048575, s.substring(sp, sp += (i >>> 20) & 63), s.substring(sp, sp += (i >>> 26) & 63);
            i = m.getUint32(p);
            p += 2;
            value = s.substring(sp, sp += i & 65535);
            nodes[id].setAttributeNS(namespace, name, value);
            break;
        case 3:
            i = m.getUint32(p);
            p += 4;
            id, name = i & 1048575, s.substring(sp, sp += (i >>> 20) & 255);
            nodes[id] = document.createElement(name);
            break;
        case 4:
            i = m.getUint32(p);
            p += 2;
            namespace, name = s.substring(sp, sp += i & 255), s.substring(sp, sp += (i >>> 8) & 255);
            nodes[id] = document.createElementNS(namespace, name);
            break;
        case 5:
            i = m.getUint32(p);
            p += 4;
            id, text = i & 1048575, s.substring(sp, sp += (i >>> 20) & 4095);
            nodes[id] = document.createTextNode(text);
            break;
        case 6:
            i = m.getUint32(p);
            p += 3;
            child = i & 1048575;
            i = m.getUint32(p);
            p += 3;
            parent = i & 1048575;
            nodes[parent].appendChild(nodes[child]);
            break;
        case 7:
            i = m.getUint32(p);
            p += 3;
            child = i & 1048575;
            i = m.getUint32(p);
            p += 3;
            parent = i & 1048575;
            nodes[parent].removeChild(nodes[child]);
            break;
        case 8:
            i = m.getUint32(p);
            p += 3;
            child = i & 1048575;
            i = m.getUint32(p);
            p += 3;
            parent = i & 1048575;
            nodes[parent].before(nodes[child]);
            break;
        case 9:
            i = m.getUint32(p);
            p += 3;
            child = i & 1048575;
            i = m.getUint32(p);
            p += 3;
            parent = i & 1048575;
            nodes[parent].after(nodes[child]);
            break;
        case 10:
            i = m.getUint32(p);
            p += 4;
            id, text = i & 1048575, s.substring(sp, sp += (i >>> 20) & 4095);
            nodes[id].textContent = text;
            break;
        case 11:
            i = m.getUint32(p);
            p += 3;
            id = i & 1048575;
            nodes[id].remove();
            break;
        case 12:
            i = m.getUint32(p);
            p += 3;
            new_id = i & 1048575;
            i = m.getUint32(p);
            p += 3;
            id = i & 1048575;
            nodes[id].replaceWith(nodes[new_id]);
            break;
        case 13:
            i = m.getUint32(p);
            p += 3;
            into = i & 1048575;
            i = m.getUint32(p);
            p += 3;
            id = i & 1048575;
            nodes[into] = nodes[id].cloneNode();
            break;
    }
}