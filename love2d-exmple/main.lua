local gm = require("GeoMan")
local lg = love.graphics

local win_w, win_h = lg.getDimensions()
local font = lg.getFont()


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
local poly_allIn_poly

function love.update()

    local mx, my = love.mouse.getPosition()

    gm.setPosition(mx,my, rpoly)

    polys_intersectAABB = gm.isPolysIntersect_AABB(hex.verts, rpoly)

    polys_intersect = gm.isPolysIntersect(hex.verts, rpoly)

    center_in_poly = gm.isPointInPoly(mx,my,hex.verts)

    poly_in_poly = gm.isPolyInPoly(rpoly, hex.verts)
    poly_allIn_poly = gm.isPolyInPoly(rpoly, hex.verts, true)

end

function love.keyreleased(key)
    if key == "space" then
        hex.verts = gm.simplifyPoly(hex.verts)
        hex.triangles = love.math.triangulate(hex.verts)
        hex.w, hex.h, hex.x, hex.y = gm.getPolyDimensions(hex.verts)
    end
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

    elseif btn == 3 then

        hex.verts = gm.keepMidVerts(hex.verts)
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

    -- Print infos --

    lg.setColor(1,1,1)

    lg.print("AABB intersction: "..tostring(polys_intersectAABB), 0,0)
    lg.print("Polygons intersection: "..tostring(polys_intersect), 0,16)
    lg.print("Center inside poly: "..tostring(center_in_poly), 0,32)
    lg.print("Poly in poly: "..tostring(poly_in_poly), 0,48)
    lg.print("Poly entirely in poly: "..tostring(poly_allIn_poly), 0,64)

    local str = "Vertices number: "..tostring(#hex.verts/2)
    local w = font:getWidth(str); lg.print(str, win_w-w, 0)

    lg.print("Left click to unite polygons.", 0, win_h-48)
    lg.print("Middle click to keep middle of edges.", 0, win_h-32)
    lg.print("Right click to apply convex hull.", 0, win_h-16)

    local str = "Press SPACE for simplify polygon."
    local w = font:getWidth(str); lg.print(str, win_w-w, win_h-16)

end
