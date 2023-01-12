// get area by territory 
// dhemerson.costa@ipam.org.br

// an adaptation from:
// calculate area of @author João Siqueira

// define root imageCollection
var root = 'users/mapbiomascerrado1/fundiario_ipam/';

// define files to process 
var asset = [
    root + 'fundiario'
  ];

// define classification regions 
var territory = ee.Image('users/dh-conciani/basemaps/ecoregions-cerrado-fip-2022');
    territory = territory.updateMask(territory.neq(128)).rename('territory');

// plot regions
Map.addLayer(territory.randomVisualizer());

// change the scale if you need.
var scale = 30;

// define the years to bem computed 
var years = ee.List.sequence({'start': 1985, 'end': 2021, 'step': 1}).getInfo();

// define a Google Drive output folder 
var driverFolder = 'AREA-EXPORT';

// for each file 
asset.map(function(file) {
  // get the classification for the file[i] 
  var asset_i = ee.Image(file).selfMask();
  
  // get no information (0) and change value to 400 (to avoid miss-masking)
  var no_info = asset_i.updateMask(asset_i.eq(0)).remap([0], [400]);
  asset_i = asset_i.blend(no_info);

  
  // set only the basename
  var basename = file.slice(ee.String(root).length().getInfo());
  
  // Image area in km2
  var pixelArea = ee.Image.pixelArea().divide(10000);
  
  // Geometry to export
  var geometry = asset_i.geometry();
  
  // convert a complex object to a simple feature collection 
  var convert2table = function (obj) {
    obj = ee.Dictionary(obj);
      var territory = obj.get('territory');
      var classesAndAreas = ee.List(obj.get('groups'));
      
      var tableRows = classesAndAreas.map(
          function (classAndArea) {
              classAndArea = ee.Dictionary(classAndArea);
              var classId = classAndArea.get('class');
              var area = classAndArea.get('sum');
              var tableColumns = ee.Feature(null)
                  .set('territory', territory)
                  .set('class_id', classId)
                  .set('area', area)
                  .set('file', basename);
                  
              return tableColumns;
          }
      );
  
      return ee.FeatureCollection(ee.List(tableRows));
  };
  
  // compute the area
  var calculateArea = function (image, territory, geometry) {
      var territotiesData = pixelArea.addBands(territory).addBands(image)
          .reduceRegion({
              reducer: ee.Reducer.sum().group(1, 'class').group(1, 'territory'),
              geometry: geometry,
              scale: scale,
              maxPixels: 1e12
          });
          
      territotiesData = ee.List(territotiesData.get('groups'));
      var areas = territotiesData.map(convert2table);
      areas = ee.FeatureCollection(areas).flatten();
      return areas;
  };
  
  // perform per year 
  var areas = years.map(
      function (year) {
        // get lcluc
        var lulc = ee.Image('projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2')
                    .select(['classification_' + year]);
        
          var image = asset_i.updateMask(lulc.eq(11));
          Map.addLayer(image.randomVisualizer(), {}, 'tenure_wet ' + year, false);
          
          var areas = calculateArea(image, territory, geometry);
          // set additional properties
          areas = areas.map(
              function (feature) {
                  return feature.set('year', year);
              }
          );
          return areas;
      }
  );
  
  areas = ee.FeatureCollection(areas).flatten();
  
  Export.table.toDrive({
      collection: areas,
      description: basename,
      folder: driverFolder,
      fileFormat: 'CSV'
  });
});
