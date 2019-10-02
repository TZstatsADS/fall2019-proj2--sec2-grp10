import json
import pandas as pd
from shapely.geometry import shape, Point

with open("nyc-zip-code.json", 'r') as f:
    datastore = json.load(f)
    
zipcode_data = datastore["features"]
no_zipcode = []


df = pd.read_csv('NYPD_Shooting_Incident_Data__Historic_.csv')

zipcode_result = []
for i in range(len(df)):
    point_i = Point(df["Longitude"][i],df["Latitude"][i])
    tag = False
    distance = {}
    for j in range(len(zipcode_data)):
        polygon = shape(zipcode_data[j]["geometry"])
        if polygon.contains(point_i):
            zipcode_result.append(zipcode_data[j]["properties"]["postalCode"])
            tag = True
            break
        else:
            distance[zipcode_data[j]["properties"]["postalCode"]] = polygon.exterior.distance(point_i)
    if tag==False:
        key_min = min(distance.keys(), key=(lambda k: distance[k]))
        zipcode_result.append(key_min)
        if distance[key_min]>=1e-4:
            print(i,point_i,key_min,distance[key_min])
            zipcode_result[i]=0

df["Zipcode"] = zipcode_result

df1 = df[df['Zipcode']!=0]

df1.to_csv('NYPD_Shooting_modified.csv')