import ee
import pandas as pd

out = pd.read_csv("D:\Documents and Settings\mcooper\Google Drive\Feed the Future\Coords&Dates.csv")

ee.Initialize()

def rename_dict(pref, d):
    for i in d.keys():
        d[pref + i] = d.pop(i)
    return(d)

def merge_dicts(*dicts):
    superdict = {}
    for d in dicts:
        for k, v in d.iteritems():
            superdict[k] = v
    return(superdict)

y2011 = []
y2012 = []
y2015 = []

for row in out.iterrows():
    geom = ee.Geometry.Point(row[1]['longitude'], row[1]['latitude']).buffer(7500)
    feat = ee.Feature(geom, {'hh_refno':row[1]['hh_refno'], 'year':str(row[1]['year'])})
    if row[1]['year'] == 2011:
        y2011.append(feat)
    elif row[1]['year'] == 2012:
        y2012.append(feat)
    elif row[1]['year'] == 2015:
        y2015.append(feat)
    else:
        raise ValueError('Invalid year.  Must be 2011, 2012, or 2015')

y2011 = ee.FeatureCollection(y2011)
y2012a = ee.FeatureCollection(y2012[:3000])
y2012b = ee.FeatureCollection(y2012[3000:6000])
y2012c = ee.FeatureCollection(y2012[6000:9000])
y2012d = ee.FeatureCollection(y2012[9000:12000])
y2012e = ee.FeatureCollection(y2012[12000:])

y2015a = ee.FeatureCollection(y2015[:3000])
y2015b = ee.FeatureCollection(y2015[3000:6000])
y2015c = ee.FeatureCollection(y2015[6000:9000])
y2015d = ee.FeatureCollection(y2015[9000:12000])
y2015e = ee.FeatureCollection(y2015[12000:])

y2011 = [y2011]
y2012 = [y2012a, y2012b, y2012c, y2012e, y2012e]
y2015 = [y2015a, y2015b, y2015c, y2015d, y2015e]

############################
#Get all cci land cover
############################
cci11 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2011-v207")
cci12 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2012-v207")
cci15 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v207")

cci11r = map(lambda(x): cci11.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), y2011)
cci12r = map(lambda(x): cci12.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), y2012)
cci15r = map(lambda(x): cci15.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), y2015)

cciaccum = pd.DataFrame()
for f in cci11r + cci12r + cci15r:
    for i in f['features']:
        temp = pd.DataFrame(merge_dicts(rename_dict('cci_', i['properties']['histogram']),
                                        {'hh_refno': i['properties']['hh_refno']},
                                        {'year': i['properties']['year']}), index = [0])
        cciaccum = cciaccum.append(temp)
    
############################
#Get total PA area
############################
#Get PAs
PA = ee.Image('users/mcooper/wdpa')

PA11r = map(lambda(x): PA.where(PA.eq(1),0).where(PA.eq(0), 1).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), y2011)
PA12r = map(lambda(x): PA.where(PA.eq(1),0).where(PA.eq(0), 1).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), y2012)
PA15r = map(lambda(x): PA.where(PA.eq(1),0).where(PA.eq(0), 1).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=x).getInfo(), y2015)

paaccum = pd.DataFrame()
for p in PA11r + PA12r + PA15r:
    for i in p['features']:
        temp = pd.DataFrame(merge_dicts(rename_dict('PA_', i['properties']['histogram']),
                                        {'hh_refno': i['properties']['hh_refno']},
                                        {'year': i['properties']['year']}), index = [0])
        paaccum = paaccum.append(temp)

        
###############################
#Get population density
#############################

pop10 = ee.Image("CIESIN/GPWv4/population-count/2010")
pop15 = ee.Image("CIESIN/GPWv4/population-count/2015")

pop10r = map(lambda(x): pop10.reduceRegions(reducer=ee.Reducer.sum(), collection=x).getInfo(), y2011 + y2012)
pop15r = map(lambda(x): pop15.reduceRegions(reducer=ee.Reducer.sum(), collection=x).getInfo(), y2015)

popaccum = pd.DataFrame()
for p in pop10r + pop15r:
    for i in p['features']:
        temp = pd.DataFrame({'hh_refno': i['properties']['hh_refno'], 
                             'pop': i['properties']['sum'], 
                             'year': i['properties']['year']}, index=[0])
        popaccum = popaccum.append(temp) 

###############################
#Get market distance
#############################
mkt = ee.Image("Oxford/MAP/accessibility_to_cities_2015_v1_0")

mktr = map(lambda(x): mkt.reduceRegions(reducer=ee.Reducer.mean(), collection=x).getInfo(), y2011 + y2012 + y2015)

mktaccum = pd.DataFrame()
for m in mktr:
    for i in m['features']:
        temp = pd.DataFrame({'hh_refno': i['properties']['hh_refno'], 
                             'market': i['properties']['mean'],
                             'year': i['properties']['year']}, index=[0])
        mktaccum = mktaccum.append(temp) 

##################################
#Combine and write
##################################
hh = reduce(lambda x, y: pd.merge(x, y, on=['hh_refno', 'year']), [cciaccum, paaccum, popaccum, mktaccum])

hh.to_csv("D:\Documents and Settings\mcooper\Google Drive\Feed the Future\Landcover.csv", index=False)