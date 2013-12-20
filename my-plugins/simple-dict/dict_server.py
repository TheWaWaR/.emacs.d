#!/usr/bin/env python
#coding: utf-8

from datetime import datetime
import socket

THE_DICT = {}
LOOKED_UPS = {}
HOST = '0.0.0.0'
PORT = 7373

def load_dict():
    with open('endict.txt') as f:
        for line in f:
            word, explane = line.split('\t', 1)
            THE_DICT[word] = ' '.join(explane.split('\t'))

            
def start_server():
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind((HOST, PORT))
    sock.listen(1)
    while True:
        conn, addr = sock.accept()
        word = conn.recv(1024).lower()
        explanation = THE_DICT.get(word, '.')
        if explanation != '.' and LOOKED_UPS.get(word, False) == False:
            LOOKED_UPS[word] = True
            print '[%s]: %s ==> %s' % (datetime.now().strftime('%Y-%m-%d %H:%M'),
                                       word, explanation[:-1])
        conn.sendall(explanation)
        conn.close()
        
            
if __name__ == '__main__':
    load_dict()
    start_server()

