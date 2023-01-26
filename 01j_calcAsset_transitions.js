// get wetland trajectory by each time-point change (t1, t2, t3...)
// dhemerson.costa@ipam.org.br

// read clark trajctories for wetland class, from 1985 to 2021
var clark_traj = ee.Image('users/dh-conciani/basemaps/col7_trajectories_wetland_cerrado');

// read number of changes
var n_changes = ee.Image('users/dh-conciani/basemaps/col7_nChanges_wetland_cerrado');

Map.addLayer(clark_traj, {palette: [
   "#ffffff", //[0] Mask 
   "#941004", //[1] Loss without Alternation (Pr-Ab Ch=1)
   "#020e7a", //[2] Gain without Alternation (Ab-Pr Ch=1)
   "#f5261b", //[3] Loss with Alternation    (Pr-Ab Ch>2)
   "#14a5e3", //[4] Gain with Alternation    (Ab-Pr Ch>2)
   "#ffff00", //[5] All Alternation          (Ab-Ab or Pr-Pr Ch>1)
   "#666666", //[6] Stable Presence          (Pr-Pr Ch=0)
   "#c4c3c0", //[7] Stable Absence           (Ab-Ab Ch=0)]})
   ], min:0, max: 7}, 
   'Wetland Trajectories [Clark]');

// read mapbiomas collection 
var mapbiomas = ee.Image('projects/mapbiomas-workspace/public/collection7/mapbiomas_collection70_integration_v2');

// compute by year transitions
var recipe = ee.Image([]);
ee.List.sequence({'start': 1986, 'end': 2021}).getInfo().forEach(
  function(year_i){
    // get mapbiomas in the year i
    var mapbiomas_i = mapbiomas.select(['classification_' + (year_i)]);
    // get mapbiomas in the previous year (i-1)
    var mapbiomas_ip = mapbiomas.select(['classification_' + (year_i - 1)]);
    // get transitions (from y-1 to y)
    var transitions_i = mapbiomas_ip.multiply(1000).add(mapbiomas_i)
      // remove wetland stable absence (Ab-Ab Ch=0);
      .updateMask(clark_traj.neq(7))
      // remove wetland stable presence (Pr-Pr Ch=0);
      .updateMask(clark_traj.neq(6))
      // rename to year i
      .rename('transitions_' + year_i);
    // bind into recipe
    recipe = recipe.addBands(transitions_i);
  });

///////////////////////////////////////////////////////////////////////
// example on how to translate from/to 
//var from = 11;
//var to = 33;
//var value = from * 1000 + to;
//
//print('from', ee.Number(value/1000).round());
//print('to', ee.Number(value/1000).mod(1).multiply(1000).round());
//////////////////////////////////////////////////////////////////////

// check results
print('output collection', recipe);

// export as asset
Export.image.toAsset({
  "image": recipe,
  "description": 'col7_transitions_wetland_cerrado',
  "assetId": 'users/dh-conciani/basemaps/col7_transitions_wetland_cerrado',
  "scale": 30,
  "pyramidingPolicy": {
  '.default': 'mode'
  },
  "maxPixels": 1e13,
  "region": clark_traj.geometry()
});  
