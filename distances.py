#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 19 14:50:41 2019

@author: antongaev
"""
import re
import json
import csv
import requests

route = []
latitudeX = []
longitudeX = []
latitudeY = []
longitudeY = []

#create list for jsons
data = []

reader = csv.reader(open('distance_search.csv', 'r', encoding="utf-8", newline =''))
#convert reader into json
for row in reader:
            h, k, v, a, b, c, e = row
            route.append(v)
            latitudeX.append(a)
            longitudeX.append(b)
            latitudeY.append(c)
            longitudeY.append(e)

with open('here_api_keys.json') as json_file:
    here_api_keys = json.load(json_file)
    print(here_api_keys.get('app_id'))

u = 'https://route.api.here.com/routing/7.2/calculateroute.json?app_id='
u_id = '&app_code='
u_code = '&waypoint0=geo!'
r = '&waypoint1=geo!'
l = '&mode=fastest;car;traffic:disabled'
comma = ','

for i in range(1,len(route)):
    try:
        url = u+here_api_keys.get('app_id')+u_id+here_api_keys.get('app_code')+u_code+latitudeX[i]+comma+longitudeX[i]+r+latitudeY[i]+comma+longitudeY[i]+l
        response = requests.get(url).text
        rus = json.loads(response)
        elem = int(int(rus.get("response").get("route")[0].get('summary').get('distance')) / 1000)
        datasuka = {"distance": elem, "route": route[i]}
        data.append(datasuka)
    except:
        datasuka = {"distance": "NA", "route": route[i]}
        data.append(datasuka)

#finally json saver should be added
with open('parsed_distances.json', 'w') as fp:
            json.dump(data, fp)
