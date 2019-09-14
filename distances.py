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

u = 'https://route.api.here.com/routing/7.2/calculateroute.json?app_id=IU3YYj0Ai0H8AVHef3Rp&app_code=73UE_IH4hNxcEohNdQ0Zdw&waypoint0=geo!'
r = '&waypoint1=geo!'
l = '&mode=fastest;car;traffic:disabled'
comma = ','

for i in range(1,len(route)):
    try:
        url = u+latitudeX[i]+comma+longitudeX[i]+r+latitudeY[i]+comma+longitudeY[i]+l
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
