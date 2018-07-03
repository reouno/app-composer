#!/usr/bin/env python3
#-*- coding:UTF-8 -*-

import argparse
import MeCab
from json import dumps, loads
from typing import Text

def wakati(text: Text) -> Text:
  tagger = MeCab.Tagger('-Owakati')
  return tagger.parse(text)

def wakatiJson(jsonData: Text) -> Text:
  text = loads(jsonData)['text']
  wakatiText = wakati(text)
  return dumps({'wakati':wakatiText})

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('json')
  a = parser.parse_args()
  if 0:
    with open('/Users/reo/Desktop/log.txt', 'a') as f:
      f.write('input to wakati.py:\n{}\n\n'.format(a.json))
  print(wakatiJson(a.json))
