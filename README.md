
# GeoMan

GeoMan is a geometry library written in Lua that allows you to generate shapes, do collision tests, cut concave shapes into convex shapes and get lots of other useful data.

# Why did I do this ?

I started this project from that of [AlexarJING](https://github.com/AlexarJING/polygon/) and I gradually added features until giving this library.

# Other mentions

I also included the library [polypartition](https://github.com/ivanfratric/polypartition) originally written by [ivanfratric](https://github.com/ivanfratric) then rewritten by [apicici](https://github.com/apicici) in MoonScript for his program [pathfun-editor](https://github.com/apicici/pathfun-editor).

There is also a Lua rewrite of the [simplify-js](https://github.com/mourner/simplify-js) module to simplify polygons. I published the Lua port alone [here](https://github.com/Bigfoot71/simplify-lua).

# List of functions

    ellipse                 (x,y,rx,ry,seg)
    circle                  (x,y,s,seg)
    hexagon	                (x,y,r)
    lozenge                 (x,y,w,h)
    random                  (count, size)
    keepMidPoints           (points, keepOlds)
    getSegDist              (x,y, x1,y1,x2,y2)
    simplifyPoly            (points, tolerance, highestQuality)
    getPolyArea    	        (verts)
    getPolyDimensions       (verts)
    getPolyLength           (verts, isLine)
    convexHull              (verts)
    concaveHull             (verts)
    translate   	        (x,y, rot, size, verts)
    newTranslated           (x,y, rot, size, verts)
    getMiddle               (x1,y1, x2,y2)
    getDist                 (x1,y1, x2,y2)
    getAngle                (x1,y1, x2,y2, toggle)
    setPolyPosition         (points, x,y, isCenter)
    isPolySelfIntersect     (verts)
    isPolysIntersect_AABB   (verts1, verts2)
    isPolysIntersect        (verts1, verts2)
    isPolyInPoly            (verts1, verts2, entirely)
    isPointInPoly           (x,y, verts)
    isCircleInPoly          (x,y,r, verts, isOut)
    isCircleOutPoly         (x,y,r, verts)
    isCircleInCircle        (x1,y1,r1, x2,y2,r2, repos)
    isCircleOutCircle       (x1,y1,r1, x2,y2,r2, repos)
    isTrisIntersect         (tri1, tri2)
    isPointInTri            (x,y, triangle)
    getPolysSharedEdges     (verts1, verts2)
    isPolysAdjacents,       (verts1, verts2, getAdj, round_v)
    getTriArea              (triangle)
    getTriCenter            (triangle)
    getAdjacentTris         (index, triangles)
    isBetween               (x,y, x1,y1,x2,y2)
    linesIntersect          (x1,y1,x2,y2, x3,y3,x4,y4)
    isSegmentsOverlap       (x1,y1,x2,y2,x3,y3,x4,y4)
    nearestPoint            (x,y, points)
    lineCross               (x1,y1,x2,y2, x3,y3,x4,y4)
    polybool                (p1,p2, operator, getMostVerts)
    convexpart              (verts)
