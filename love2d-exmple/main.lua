local gm = require("GeoMan")
local lg = love.graphics

local win_w, win_h = lg.getDimensions()


-- Gen hexagon --

local hex = {
    verts = gm.hexagon(
        (win_w - 100) / 2,
        (win_h - 100) / 2,
        100
    )
}

hex.triangles = love.math.triangulate(hex.verts)
hex.w, hex.h, hex.x, hex.y = gm.getPolyDimensions(hex.verts)


-- Gen random shape who will follow the mouse --

math.randomseed(os.time()) -- To be sure to have a new shape every time
local rpoly = gm.random(0,0,4,1)


-- Main program --

local polys_intersectAABB
local polys_intersect
local center_in_poly
local poly_in_poly

function love.update()

    local mx, my = love.mouse.getPosition()

    gm.setPosition(mx,my, rpoly)

    polys_intersectAABB = gm.isPolysIntersect_AABB(hex.verts, rpoly)
    polys_intersect = gm.isPolysIntersect(hex.verts, rpoly)
    center_in_poly = gm.isPointInPoly(mx,my,hex.verts)
    poly_in_poly = gm.isPolyInPoly(rpoly, hex.verts)

end

function love.mousereleased(x,y,btn)

    if btn == 1 and polys_intersect then

        hex.verts = gm.polybool(hex.verts, rpoly, "or")
        hex.triangles = love.math.triangulate(hex.verts)
        hex.w, hex.h, hex.x, hex.y = gm.getPolyDimensions(hex.verts)
        rpoly = gm.random(0,0,4,1)

    elseif btn == 2 then

        hex.verts = gm.convexHull(hex.verts)
        hex.triangles = love.math.triangulate(hex.verts)
        hex.w, hex.h, hex.x, hex.y = gm.getPolyDimensions(hex.verts)

    end

end

function love.draw()

    -- Draw hexgon with rectangle arround and "origin point" --

    lg.setColor(0,0,1)

    for _, tri in pairs(hex.triangles) do
        lg.polygon("fill", tri)
    end

    lg.rectangle("line", hex.x, hex.y, hex.w, hex.h)

    -- Draw a shape that follows the mouse --

    lg.setColor(1,0,0)

    lg.polygon("fill", rpoly)
    local w,h,x,y = gm.getPolyDimensions(rpoly)
    lg.rectangle("line", x, y, w, h)

    -- Print collision infos --

    lg.setColor(1,1,1)

    lg.print("AABB intersction: "..tostring(polys_intersectAABB), 0,0)
    lg.print("Polygons intersection: "..tostring(polys_intersect), 0,16)
    lg.print("Center inside poly: "..tostring(center_in_poly), 0,32)
    lg.print("Poly all in poly: "..tostring(poly_in_poly), 0,48)

    lg.print("Left click to unite polygons.", 0, win_h-32)
    lg.print("Right click to apply convex hull.", 0, win_h-16)

end
