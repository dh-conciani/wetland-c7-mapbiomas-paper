// get collection 7.
var lulc = ee.Image('projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2');

// get secondary vegetation age
var secondary_age = ee.Image('projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_secondary_vegetation_age_v2');

// get monthly water (from GT)
var water = ee.ImageCollection('projects/mapbiomas-workspace/TRANSVERSAIS/AGUA5-FT')
              .filterMetadata('version', 'equals', '11')
              .filterMetadata('biome', 'equals', 'CERRADO')
              .filterMetadata('cadence', 'equals', 'monthly');

print(water, 'water')


var i = ee.Image('projects/mapbiomas-workspace/TRANSVERSAIS/AGUA5-FT/CERRADO-1985-11_mensal')
print(i, 'image')

i = i.reduce(ee.Reducer.sum())
print(i, 'image 2')

Map.addLayer(i.randomVisualizer())
