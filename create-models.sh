lein run src/dactyl_keyboard/dactyl.clj
/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD -o things/right-cyril.stl things/right-cyril.scad
/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD -o things/left-cyril.stl  things/left-cyril.scad
/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD -o things/right-plate-cyril.stl things/right-plate-cyril.scad
/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD -o things/left-plate-cyril.stl  things/left-plate-cyril.scad

# patch -p1 < 4x6.patch 
# lein run src/dactyl_keyboard/dactyl.clj
# cp things/right.scad things/right-4x6.scad
# cp things/left.scad things/left-4x6.scad
# cp things/right-plate.scad things/right-4x6-plate.scad
# openscad -o things/right-4x6-plate.dxf things/right-4x6-plate.scad >/dev/null 2>&1 &
# openscad -o things/right-4x6.stl things/right-4x6.scad >/dev/null 2>&1  &
# openscad -o things/left-4x6.stl  things/left-4x6.scad >/dev/null 2>&1 &
# git checkout src/dactyl_keyboard/dactyl.clj

# patch -p1 < 5x6.patch 
# lein run src/dactyl_keyboard/dactyl.clj
# cp things/right.scad things/right-5x6.scad
# cp things/left.scad things/left-5x6.scad
# cp things/right-plate.scad things/right-5x6-plate.scad
# openscad -o things/right-5x6-plate.dxf things/right-5x6-plate.scad >/dev/null 2>&1 &
# openscad -o things/right-5x6.stl things/right-5x6.scad >/dev/null 2>&1  &
# openscad -o things/left-5x6.stl  things/left-5x6.scad >/dev/null 2>&1 &
# git checkout src/dactyl_keyboard/dactyl.clj

# patch -p1 < 6x6.patch 
# lein run src/dactyl_keyboard/dactyl.clj
# cp things/right.scad things/right-6x6.scad
# cp things/left.scad things/left-6x6.scad
# cp things/right-plate.scad things/right-6x6-plate.scad
# openscad -o things/right-6x6-plate.dxf things/right-6x6-plate.scad >/dev/null 2>&1 &
# openscad -o things/right-6x6.stl things/right-6x6.scad >/dev/null 2>&1  &
# openscad -o things/left-6x6.stl  things/left-6x6.scad >/dev/null 2>&1 &
# git checkout src/dactyl_keyboard/dactyl.clj


# git add things/*-4x5.stl
# git add things/right-4x5-plate.dxf
# git commit -m "Add CAD files"
# wait