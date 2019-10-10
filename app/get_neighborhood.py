import json
import pandas as pd
from shapely.geometry import shape, Point

with open("Neighborhood Tabulation Areas.json", 'r') as f:
    datastore = json.load(f)
    
neighborhood_data = datastore["features"]

df = pd.read_csv('NYPD_Shooting_Incident_Data__Historic_.csv')

neighborhood_result = []

for i in range(len(df)):
    point_i = Point(df["Longitude"][i],df["Latitude"][i])
    tag = False
    distance = {}
    for j in range(len(neighborhood_data)):
        polygon = shape(neighborhood_data[j]["geometry"])
        if polygon.contains(point_i):
            neighborhood_result.append(neighborhood_data[j]["properties"]["ntaname"])
            tag = True
            break
    if tag== False and i==18770:
        neighborhood_result.append("Woodlawn-Wakefield")

df["ntaname"] = neighborhood_result
df.to_csv('NYPD_Shooting_modified1.csv')