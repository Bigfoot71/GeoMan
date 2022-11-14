
# GeoMan

GeoMan is a geometry library written in Lua that allows you to generate shapes, do collision tests, cut concave shapes into convex shapes and get lots of other useful data.

# Why did I do this ?

I started this project from that of [AlexarJING](https://github.com/AlexarJING/polygon/) and I gradually added features until giving this library.

Here is the list of changes I made: https://pastebin.com/mjb0FsE9

# Other mentions

I also included the library [polypartition](https://github.com/ivanfratric/polypartition) originally written by [ivanfratric](https://github.com/ivanfratric) then rewritten by [apicici](https://github.com/apicici) in MoonScript for his program [pathfun-editor](https://github.com/apicici/pathfun-editor).

# List of functions

    ellipse                 (x,y,rx,ry,segments)
    circle                  (x,y,s,segments)
    hexagon                 (x,y,r)
    lozenge                 (x,y,w,h)
    random                  (count,size)
    keepMidVerts            (verts)
    getSegDist              (x,y,x1,y1,x2,y2)
    simplifyPoly            (verts,tolerance,highQuality)
    getArea                 (verts)
    getPolyDimensions       (verts)
    getTriArea              (verts)
    convexHull              (verts)
    concaveHull             (verts)
    translate               (x,y,rot,size,verts)
    newTranslated           (x,y,rot,size,verts)
    getDist                 (x1,y1,x2,y2)
    getAngle                (x1,y1,x2,y2,toggle)
    setPosition             (x,y,verts)
    isPolysIntersect_AABB   (verts1, verts2)
    isPolysIntersect        (verts1, verts2)
    isPolyInPoly            (verts1, verts2)
    isPointInPoly           (x,y,verts)
    isCircleInPoly          (x,y,r,verts,isOut)
    isCircleOutPoly         (x,y,r,verts)
    isCircleInCircle        (x1,y1,r1,x2,y2,r2,repos)
    isCircleOutCircle       (x1,y1,r1,x2,y2,r2,repos)
    isPointInTri            (x,y,verts)
    getTriCenter            (verts)
    getAdjacentTris         (index,triangles)
    isBetween               (x,y,x1,y1,x2,y2)
    linesIntersect          (x1,y1,x2,y2,x3,y3,x4,y4)
    nearestPoint            (x,y,verts)
    lineCross               (x1,y1,x2,y2,x3,y3,x4,y4)
    polybool                (p1,p2,operation)
    convexpart              (verts)
