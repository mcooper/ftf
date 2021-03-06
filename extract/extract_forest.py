import ee
import pandas as pd
import time

ee.Initialize()

buffersize = 15000

data = pd.read_csv("G:\My Drive\Feed the Future\Coords&Dates.csv")

data = data[['hh_refno', 'latitude', 'longitude', 'year']].drop_duplicates()

forest = ee.Image("UMD/hansen/global_forest_change_2016_v1_4")
baseforest = forest.select('treecover2000')
baseforest = baseforest.where(baseforest.gte(10), 1).where(baseforest.lt(10), 0)

gain = forest.select('gain')

def tryReduceRegions(raster, feature):
    try:
        reduction = raster.reduceRegions(reducer=ee.Reducer.mean(), collection=feature).getInfo()
        time.sleep(1)
    except ee.ee_exception.EEException:
        print('Memory Exceeded, waiting')
        time.sleep(60*5)
        reduction = tryReduceRegions(raster, feature)
    return reduction

accum = pd.DataFrame()
for y in range(11, 16):
    print("****************************\nNow Running Year " + str(2000 + y) + "\n****************************")
    
    loss = baseforest.where(forest.select('loss').lte(y), 1).where(forest.select('loss').gt(y), 0).where(forest.select('loss').eq(0), 0)
    
    if y > 7:
        ygain = gain
    else:
        ygain = gain.where(gain.eq(1), 0)
    
    yforest = baseforest.subtract(loss).add(gain)
    yforest = yforest.where(yforest.gt(1), 1).where(yforest.lt(0), 0)
    
    if y != 0:
        sel = data[data['year'] == (2000 + y)]
    elif y == 0:
        sel = data[data['year'] <= 2000]
    
    print("Make buffered points")
    points = []
    for row in sel.iterrows():
        if not (row[1]['longitude']==0) & (row[1]['latitude']==0):
            geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(buffersize)
            feat = ee.Feature(geom, {'hh_refno':row[1]['hh_refno']})
            points.append(feat)
    
    print("Make features")
    feat = []
    i = 0
    while i < len(points):
        j = i + 50
        fc = ee.FeatureCollection(points[i:j])
        feat.append(fc)
        i = j
    
    print("Reduce regions")
    summary = []
    for f in feat:
        summary.append(tryReduceRegions(yforest, f))
        print(str(feat.index(f)) + " of " + str(len(feat)) + " in " + str(2000 + y))
    
    print("Add to DF")
    for featclass in summary:   
        feats = featclass['features']
        for f in feats:
            hh_refno = f['properties']['hh_refno']
            mean = f['properties']['mean']
            accum = accum.append(pd.DataFrame({'hh_refno': hh_refno, 'mean': mean, 'year': (2000 + y)}, index = [0]))
    
    time.sleep(60)

accum.drop_duplicates() \
.groupby('hh_refno', as_index=False)['mean'].mean() \
.rename(columns = {'mean': 'forest_' + str(buffersize)}) \
.to_csv('forest_cover' + str(buffersize) + '.csv', index=False)
