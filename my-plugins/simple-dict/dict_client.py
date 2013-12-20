#!/usr/bin/env python
#coding: utf-8

import sys
import socket

HOST = '127.0.0.1'
PORT = 7373

def lookup(word):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((HOST, PORT))
    sock.sendall(word)
    data = sock.recv(1024)
    sock.close()
    print data,


if __name__ == '__main__':
    if len(sys.argv) < 2:
        sys.exit()
    lookup(sys.argv[1])
