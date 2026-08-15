// tests/ws_server.js — WebSocket + SSE servers for tests/websocket_test.ag
// and tests/sse_test.ag.
//
//   - ws://127.0.0.1:18446/echo  — RFC 6455 echo (text/binary), ping->pong,
//     close handshake echo. Server frames are never masked.
//   - ws://127.0.0.1:18446/frag  — sends a fragmented text message then a
//     ping, then waits for close.
//   - ws://127.0.0.1:18446/reject — upgrades are refused (HTTP 403).
//   - http://127.0.0.1:18446/sse — Server-Sent Events: three events then
//     the [DONE] marker, chunked transfer encoding.
//   - http://127.0.0.1:18446/chunked — a chunked response with two chunks.
//
// Used by run_tests.sh only when node is available.

const http = require('http');
const crypto = require('crypto');

const WS_GUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

function buildFrame(opcode, payload, fin) {
    if (fin === undefined) fin = true;
    const b0 = (fin ? 0x80 : 0) | opcode;
    const len = payload.length;
    let header;
    if (len < 126) {
        header = Buffer.from([b0, len]);
    } else if (len <= 0xffff) {
        header = Buffer.from([b0, 126, (len >> 8) & 0xff, len & 0xff]);
    } else {
        header = Buffer.alloc(10);
        header[0] = b0;
        header[1] = 127;
        let n = BigInt(len);
        for (let i = 9; i >= 2; i--) {
            header[i] = Number(n & 0xffn);
            n >>= 8n;
        }
    }
    return Buffer.concat([header, payload]);
}

// Incremental client-frame parser: yields { fin, opcode, payload } frames.
function makeFrameParser(onFrame) {
    let buf = Buffer.alloc(0);
    function process() {
        while (true) {
            if (buf.length < 2) return;
            const b0 = buf[0];
            const b1 = buf[1];
            const fin = (b0 & 0x80) !== 0;
            const opcode = b0 & 0x0f;
            const masked = (b1 & 0x80) !== 0;
            let len = b1 & 0x7f;
            let off = 2;
            if (len === 126) {
                if (buf.length < 4) return;
                len = (buf[2] << 8) | buf[3];
                off = 4;
            } else if (len === 127) {
                if (buf.length < 10) return;
                let n = 0n;
                for (let i = 2; i < 10; i++) n = (n << 8n) | BigInt(buf[i]);
                len = Number(n);
                off = 10;
            }
            let mask = null;
            if (masked) {
                if (buf.length < off + 4) return;
                mask = buf.slice(off, off + 4);
                off += 4;
            }
            if (buf.length < off + len) return;
            let payload = buf.slice(off, off + len);
            if (masked) {
                const unmasked = Buffer.alloc(len);
                for (let i = 0; i < len; i++) unmasked[i] = payload[i] ^ mask[i % 4];
                payload = unmasked;
            }
            buf = buf.slice(off + len);
            onFrame({ fin, opcode, payload });
        }
    }
    return {
        push(data) {
            buf = Buffer.concat([buf, data]);
            process();
        },
    };
}

const server = http.createServer((req, res) => {
    if (req.url === '/sse') {
        res.writeHead(200, {
            'content-type': 'text/event-stream',
            'cache-control': 'no-cache',
            'transfer-encoding': 'chunked',
        });
        res.write('event: greet\nid: 1\ndata: hello sse\n\n');
        res.write('data: line one\ndata: line two\n\n');
        res.write(': a comment\n\n');
        res.write('event: end\ndata: [DONE]\n\n');
        res.end();
    } else if (req.url === '/chunked') {
        res.writeHead(200, { 'content-type': 'text/plain', 'transfer-encoding': 'chunked' });
        res.write('chunk-one;');
        res.write('chunk-two');
        res.end();
    } else {
        res.writeHead(404, { 'content-type': 'text/plain' });
        res.end('not found');
    }
});

server.on('upgrade', (req, socket, head) => {
    const key = req.headers['sec-websocket-key'];
    if (req.url !== '/echo' && req.url !== '/frag') {
        socket.write('HTTP/1.1 403 Forbidden\r\nConnection: close\r\n\r\n');
        socket.end();
        return;
    }
    if (!key) {
        socket.write('HTTP/1.1 400 Bad Request\r\nConnection: close\r\n\r\n');
        socket.end();
        return;
    }
    const accept = crypto.createHash('sha1').update(key + WS_GUID).digest('base64');
    socket.write(
        'HTTP/1.1 101 Switching Protocols\r\n' +
        'Upgrade: websocket\r\n' +
        'Connection: Upgrade\r\n' +
        `Sec-WebSocket-Accept: ${accept}\r\n` +
        '\r\n'
    );
    if (head && head.length) socket.unshift(head);

    if (req.url === '/frag') {
        // Fragmented text message: "hel" + "lo" (continuation), then a ping.
        socket.write(buildFrame(0x1, Buffer.from('hel'), false));
        socket.write(buildFrame(0x0, Buffer.from('lo'), true));
        socket.write(buildFrame(0x9, Buffer.from('frag-ping')));
    }

    const parser = makeFrameParser((frame) => {
        if (frame.opcode === 0x8) {
            // Close: echo the close frame, then end.
            socket.write(buildFrame(0x8, frame.payload));
            socket.end();
            return;
        }
        if (frame.opcode === 0x9) {
            socket.write(buildFrame(0xa, frame.payload));
            return;
        }
        if (frame.opcode === 0x1 || frame.opcode === 0x2) {
            socket.write(buildFrame(frame.opcode, frame.payload));
            return;
        }
        // Continuation frames on /echo are treated as standalone echoes.
    });
    socket.on('data', (chunk) => parser.push(chunk));
    socket.on('error', () => {});
});

server.listen(18446, '127.0.0.1', () => console.log('WS_NODE_READY'));
