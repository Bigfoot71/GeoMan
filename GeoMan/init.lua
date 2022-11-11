local path = ...

local Delaunay = require(path.."/delaunay")

local polybool   = require(path.."/polybool")
local convexpart = require(path.."/convexpart")
local lineCross  = require(path.."/lineCross")


local getDist = function(x1,y1,x2,y2)
    return math.sqrt((x1-x2)^2+(y1-y2)^2)
end

local getRot = function (x1,y1,x2,y2,toggle)
    if x1==x2 and y1==y2 then return 0 end
    local angle=math.atan((x1-x2)/(y1-y2))
    if y1-y2<0 then angle=angle-math.pi end
    if toggle==true then angle=angle+math.pi end
    if angle>0 then angle=angle-2*math.pi end
    if angle==0 then return 0 end
    return -angle
end

local axisRot = function(x,y,rot)
    return math.cos(rot)*x-math.sin(rot)*y,math.cos(rot)*y+math.sin(rot)*x
end

local round = function(num, decimals)
    decimals = math.pow(10, decimals or 0); num = num * decimals
    num = num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
    return num / decimals
end

local clamp = function (n,low,high)
    if low>high then return math.max(high,math.min(n,low))
    else return math.max(low,math.min(n,high)) end
end

local sign = function(x1,y1, x2,y2, x3,y3)
    return (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)
end

local ccw = function(a,b,c)
    return (b[1] - a[1]) * (c[2] - a[2]) > (b[2] - a[2]) * (c[1] - a[1])
end

local v2crossProduct = function(x1, y1, x2, y2)
    return x1 * y2 - y1 * x2
end




local function newEllipse(ox, oy, rx, ry, segments)
    ox, oy = ox or 0, oy or 0
    rx, ry = rx or 1, ry or 1
    segments = segments or 40

    local vertices = {ox, oy}

    for i=0, segments do
        local angle = (i / segments) * math.pi * 2
        local x = ox + math.cos(angle) * rx
        local y = oy + math.sin(angle) * ry
        table.insert(vertices, x)
        table.insert(vertices, y)
    end

    return vertices
end


local function newCircle(x,y,r,segments,err)
    return newEllipse(x,y,r,r,segments or round(math.pi/math.acos(1-(err or 0.33)/r)))
end


local function newHexagon(x,y,l)
    local i=(l/2)*3^0.5
    return {x,y,x+l,y,x+1.5*l,y+i,x+l,y+2*i,x,y+2*i,x-l*0.5,y+i}
end


local function newLozenge(x,y,w,h)
    local hw, hh = w/2, h/2
    return {x,y+hh, x+hw,y, x+w,y+hh, x+hw,y+h}
end


local function newPolygonTrans(x,y,rot,size,v)

    rot, size = rot or 0, size or 1

    local tab={}
    for i=1,#v/2 do
        tab[2*i-1],tab[2*i]=axisRot(v[2*i-1],v[2*i],rot)
        tab[2*i-1]=tab[2*i-1]*size+x
        tab[2*i]=tab[2*i]*size+y
    end

    return tab

end


local function polygonTrans(x,y,rot,size,v)

    rot, size = rot or 0, size or 1

    for i=1,#v/2 do
        v[2*i-1], v[2*i] = axisRot(v[2*i-1], v[2*i], rot)
        v[2*i-1] = v[2*i-1]*size+x
        v[2*i] = v[2*i]*size+y
    end

end


--[[
local function isPointInPoly(x,y,verts) -- OLD
    local pX={}
    local pY={}
    for i=1,#verts,2 do
        table.insert(pX, verts[i])
        table.insert(pY, verts[i+1])
    end
    local oddNodes=false
    local pCount=#pX
    local j=pCount
    for i=1,pCount do
        if ((pY[i]<y and pY[j]>=y) or (pY[j]<y and pY[i]>=y))
            and (pX[i]<=x or pX[j]<=x) then
            if pX[i]+(y-pY[i])/(pY[j]-pY[i])*(pX[j]-pX[i])<x then
                oddNodes=not oddNodes
            end
        end
        j=i
    end
    return oddNodes
end
]]


local function isPointInPoly(x,y,verts)

    local inside, iN = false, 1
    for iC = 1, #verts-1, 2 do

        iN = iN < #verts-1 and iN+2 or 1
        local xc, yc = verts[iC], verts[iC+1]
        local xn, yn = verts[iN], verts[iN+1]

        if ((yn > y) ~= (yc > y)) and (x < (y - yn) * (xc - xn)/(yc - yn) + xn) then
            inside = not inside
        end

    end

    return inside

end


local function isCircleInPoly(x,y,r, poly, isOut)

    local inside = isOut or false

    for i = 1, #poly-1, 2 do

        local j = i+2 < #poly and i+2 or 1

        local ix, iy = poly[i], poly[i+1]
        local jx, jy = poly[j], poly[j+1]

        local dx, dy = jx - ix, jy - iy
        local vx, vy = x - ix, y - iy

        local t = (vy * dy + vx * dx) / (dy * dy + dx * dx)
        local t = (t < 0) and 0 or (t > 1) and 1 or t

        local nx = t * dx - vx
        local ny = t * dy - vy
        local nd = nx * nx + ny * ny

        if nd < r * r then return true end

        if nx < 0 and (iy < y) ~= (jy < y) then
            inside = not inside
        end

    end

    return inside

end


local function isCircleOutPoly(x,y,r,poly,aseg) -- TODO: must be optimized

    if isCircleInPoly(x,y,r,poly,true) then

        local sin, cos = math.sin, math.cos
        aseg = aseg or math.acos(2*(1-.1/r)^2-1)

        for i = 0, math.pi*2, aseg do
            local tx = x - sin(i) * r
            local ty = y - cos(i) * r
            if not isPointInPoly(tx,ty,poly) then
                return true
            end
        end

    end

    return false

end


local function isPointInTri(x,y,verts)

    local x1, y1 = verts[1], verts[2]
    local x2, y2 = verts[3], verts[4]
    local x3, y3 = verts[5], verts[6]

    local d1 = sign(x,y, x1,y1, x2,y2)
    local d2 = sign(x,y, x2,y2, x3,y3)
    local d3 = sign(x,y, x3,y3, x1,y1)

    local neg = (d1 < 0) or (d2 < 0) or (d3 < 0)
    local pos = (d1 > 0) or (d2 > 0) or (d3 > 0)

    return not (neg and pos)

end


local function getTriCenter(verts)
    local cx = (verts[1] + verts[3] + verts[5]) / 3
    local cy = (verts[2] + verts[4] + verts[6]) / 3
    return cx, cy
end


local function getAdjacentTris(index, tris)

    local adj = {}
    local tri2 = tris[index]

    for k, tri1 in pairs(tris) do

        if k ~= index then
            for i = 1, 6, 2 do

                local i1 = i<4 and i+2 or 1
                local i2 = i<5 and i+3 or 2
                local i3 = i1<4 and i1+2 or i<4 and i-2 or i1-2
                local i4 = i2<5 and i2+2 or i<5 and i-1 or i2-1

                if  ( (tri1[i] == tri2[i] and tri1[i+1] == tri2[i+1])
                   or (tri1[i] == tri2[i1] and tri1[i+1] == tri2[i2])
                   or (tri1[i] == tri2[i3] and tri1[i+1] == tri2[i4]) )
                and ( (tri1[i1] == tri2[i1] and tri1[i2] == tri2[i2])
                   or (tri1[i1] == tri2[i3] and tri1[i2] == tri2[i4]) )
                then
                    table.insert(adj, {tri1,k}); break
                end

            end
        end

        if #adj == 3 then break end

    end

    return adj

end


local function isBetween(x,y, x1,y1, x2,y2)
    return getDist(x1,y1, x,y) + getDist(x2,y2, x,y) == getDist(x1,y1, x2,y2)
end


--[[
local function linesIntersect(x1,y1, x2,y2, x3,y3, x4,y4)

    local dx1, dy1 = x2 - x1, y2 - y1
    local dx2, dy2 = x4 - x3, y4 - y3
    local dx3, dy3 = x1 - x3, y1 - y3
    local d = dx1*dy2 - dy1*dx2

    if d == 0 then return false end

    local t1 = (dx2*dy3 - dy2*dx3)/d
    if t1 < 0 or t1 > 1 then return false end

    local t2 = (dx1*dy3 - dy1*dx3)/d
    if t2 < 0 or t2 > 1 then return false end

    return true, x1 + t1*dx1, y1 + t1*dy1

end
]]


local function linesIntersect(x1,y1, x2,y2, x3,y3, x4,y4, asSegment)

    asSegment = (asSegment==nil) and true or asSegment

    local dx1 = x1 - x2
    local dy1 = y2 - y1
    local dx2 = x3 - x4
    local dy2 = y4 - y3

    local c1 = (x2 * y1) - (x1 * y2)
    local c2 = (x4 * y3) - (x3 * y4)

    local denom = (dy1 * dx2) - (dy2 * dx1)
    
    if denom == 0 then return false end
       
    local px = ((dx1 * c2) - (dx2 * c1)) / denom
    local py = ((dy2 * c1) - (dy1 * c2)) / denom

    if asSegment then

        local uc = ((y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1));
        local ua = (((x4 - x3) * (y1 - y3)) - (y4 - y3) * (x1 - x3)) / uc;
        local ub = (((x2 - x1) * (y1 - y3)) - ((y2 - y1) * (x1 - x3))) / uc;

        if ua >= 0 and ua <= 1 and ub >= 0 and ub <= 1 then
            return px, py
        else
            return false
        end

    end

    return px, py

end


local function nearestPoint(x,y,poly)
    local shortest_dist, nx, ny = math.huge

    for i = 1, #poly-1, 2 do
        local px, py = poly[i], poly[i+1]
        local dist = getDist(px,py, x,y)
        if dist < shortest_dist then
            shortest_dist = dist
            nx, ny = px, py
        end
    end

    return nx, ny

end


local function convexHull(poly) -- TODO: Find a way to call 'table.sort' on a "simple" table without having to convert it to a "table of table"
    if #poly == 0 then return {} end

    local pl = {}

    for i = 1, #poly-1, 2 do -- CONVERT
        table.insert(pl, {poly[i], poly[i+1]})
    end

    table.sort(pl, function(left,right)
        return left[1] < right[1]
    end)

    local h = {}

    for i,pt in pairs(pl) do
        while #h >= 2 and not ccw(h[#h-1], h[#h], pt) do
            table.remove(h,#h)
        end
        table.insert(h,pt)
    end

    local t = #h + 1
    for i=#pl, 1, -1 do
        local pt = pl[i]
        while #h >= t and not ccw(h[#h-1], h[#h], pt) do
            table.remove(h,#h)
        end
        table.insert(h,pt)
    end

    table.remove(h,#h)

    pl = {} -- RE-CONVERT
    for _, v in pairs(h) do
        table.insert(pl, v[1])
        table.insert(pl, v[2])
    end

    return pl
end


local function randomPolygon(x,y,count,size)
    local v = {}
    for i=1,count*2 do
        table.insert(v,math.random(-50,50)*size)
    end
    return newPolygonTrans(x,y,0,1,convexHull(v))
end


--return area, center
local function getArea(verts)
    local count=#verts/2
    local cx,cy=0,0
    local area = 0

    local refx,refy=0,0
    for i=1,#verts-1,2 do
        local p1x,p1y=refx,refy
        local p2x,p2y=verts[i],verts[i+1]
        local p3x = i+2>#verts and verts[1] or verts[i+2]
        local p3y = i+2>#verts and verts[2] or verts[i+3]

        local e1x= p2x-p1x
        local e1y= p2y-p1y
        local e2x= p3x-p1x
        local e2y= p3y-p1y

        local d=v2crossProduct(e1x,e1y,e2x,e2y)
        local triAngleArea=0.5*d
        area=area+triAngleArea
        cx = cx + triAngleArea*(p1x+p2x+p3x)/3
        cy = cy + triAngleArea*(p1y+p2y+p3y)/3
    end

    if area~=0 then
        cx, cy = cx/area, cy/area
        return math.abs(area),cx,cy
    end
end


local function setPolygonPosition(x,y,v)  -- TODO: must be optimized

    local _,cx,cy = getArea(v)

    local dx = cx - x
    local dy = cy - y

    for i = 1, #v-1, 2 do
        v[i] = v[i] - dx
        v[i+1] = v[i+1] - dy
    end

end


local function getPolyDimensions(poly)

    local xMin, xMax = math.huge, -math.huge
    local yMin, yMax = math.huge, -math.huge

    for i = 1, #poly-1, 2 do
        xMin = poly[i] < xMin and poly[i] or xMin
        xMax = poly[i] > xMax and poly[i] or xMax
        yMin = poly[i+1] < yMin and poly[i+1] or yMin
        yMax = poly[i+1] > yMax and poly[i+1] or yMax
    end

    local width = xMax - xMin
    local height = yMax - yMin

    return width, height, xMin, yMin, xMax, yMax

end


local function isPolysIntersect_AABB(v1, v2)

    local _,_, xMin1,yMin1, xMax1,yMax1 = getPolyDimensions(v1)
    local _,_, xMin2,yMin2, xMax2,yMax2 = getPolyDimensions(v2)

    return not (
      xMax1 < xMin2 or yMax1 < yMin2 or
      xMin1 > xMax2 or yMin1 > yMax2
    )

end


local function isPolysIntersect(v1, v2)

    for i1 = 1, #v1-1, 2 do

        local i2 = i1+2 < #v1 and i1+2 or 1

        for j1 = 1, #v2-1, 2 do

            local j2 = j1+2 < #v2 and j1+2 or 1

            if linesIntersect(
                v1[i1], v1[i1+1], v1[i2], v1[i2+1],
                v2[j1], v2[j1+1], v2[j2], v2[j2+1]
            ) then
                return true
            end

        end
    end

    return false

end


local function isPolyInPoly(v1, v2) -- TODO: can be optimized but good example

    if not isPolysIntersect(v1, v2) then

        for i = 1, #v1-1, 2 do
            if isPointInPoly(v1[i], v1[i+1], v2) then
                return true
            end
        end

    end

    return false

end


local function getTriArea(tri)

    local p = getDist(tri[1],tri[2], tri[3],tri[4])
    local q = getDist(tri[3],tri[4], tri[5],tri[6])
    local r = getDist(tri[5],tri[6], tri[1],tri[2])

    local s = (p+q+r)*.5

    return (s*(s-p)(s-q)(s-r))^.5

end


local function concaveHull(threshold,source)
    local Point    = Delaunay.Point
    local points = {}

    for i = 1, #source-1,2 do
        table.insert(points, Point(source[i],source[i+1]))
    end

    local triangles = Delaunay.triangulate(points)

    for i=#triangles,1,-1 do
        if triangles[i]:getCircumRadius()>threshold then
            table.remove(triangles, i)
        end
    end


    local edges={}
    for i,t in ipairs(triangles) do
        table.insert(edges,t.e1)
        table.insert(edges,t.e2)
        table.insert(edges,t.e3)
    end


    for i,t in ipairs(triangles) do

        for j,e in ipairs(edges) do
            if t.e1:same(e) and e~=t.e1 then
                table.remove(edges, j)	
                break
            end
        end

        for j,e in ipairs(edges) do
            if t.e2:same(e) and e~=t.e2 then
                table.remove(edges, j)
                break
            end
        end

        for j,e in ipairs(edges) do
            if t.e3:same(e) and e~=t.e3 then
                table.remove(edges, j)	
                break
            end
        end
    end

    local target={}
    table.remove(edges, 1)
    while #edges~=0 do
        local verts={edges[1].p1.x,edges[1].p1.y,edges[1].p2.x,edges[1].p2.y}
            table.insert(target, verts)
        repeat
            local test
            for i,e in ipairs(edges) do
                if e.p1.x==verts[#verts-1] and e.p1.y==verts[#verts] then
                    table.insert(verts, e.p2.x)
                    table.insert(verts, e.p2.y)
                    table.remove(edges, i)
                    test=true
                    break
                end

                if e.p2.x==verts[#verts-1] and e.p2.y==verts[#verts] then
                    table.insert(verts, e.p1.x)
                    table.insert(verts, e.p1.y)
                    table.remove(edges, i)
                    test=true
                    break
                end
            end

        until not test or #edges==0

        if not(verts[#verts-1]==verts[1] and verts[#verts]==verts[2]) then
            local x,y=verts[1],verts[2]
            verts[1],verts[2]=verts[3],verts[4]
            verts[3],verts[4]=x,y
        end
    end

    local rt = {}
    for i,v in ipairs(target) do
        local test,triangles = pcall(love.math.triangulate,v)
        if test then
            for i,v in ipairs(triangles) do
                table.insert(rt,v)
            end
        end
    end

    return rt
end




return {
    ellipse                 = newEllipse, 		        -- x,y,rx,ry,seg
    circle                  = newCircle,                -- x,y,s,seg
    hexagon                 = newHexagon, 		        -- x,y,r
    lozenge                 = newLozenge,               -- x,y,w,h
    random                  = randomPolygon, 	        -- count,size
    getArea                 = getArea, 			        -- verts
    getPolyDimensions       = getPolyDimensions,        -- verts
    getTriArea              = getTriArea,               -- verts
    convexHull              = convexHull,		        -- verts
    concaveHull             = concaveHull, 		        -- verts
    translate               = polygonTrans, 	        -- x,y,rot,size,verts
    newTranslated           = newPolygonTrans,          -- x,y,rot,size,verts
    setPosition             = setPolygonPosition,       -- x,y,verts
    isPolysIntersect_AABB   = isPolysIntersect_AABB,    -- verts1, verts2
    isPolysIntersect        = isPolysIntersect,         -- verts1, verts2
    isPolyInPoly            = isPolyInPoly,             -- verts1, verts2
    isPointInPoly           = isPointInPoly,	        -- x,y,verts
    isCircleInPoly          = isCircleInPoly,           -- x,y,r,verts,isOut
    isCircleOutPoly         = isCircleOutPoly,          -- x,y,r,verts
    isPointInTri            = isPointInTri,             -- x,y,verts
    getTriCenter            = getTriCenter,             -- verts
    getAdjacentTris         = getAdjacentTris,          -- index, triangles
    isBetween               = isBetween,                -- x,y,x1,y1,x2,y2
    linesIntersect          = linesIntersect,           -- x1,y1,x2,y2,x3,y3,x4,y4
    nearestPoint            = nearestPoint,             -- x,y,verts
    lineCross               = lineCross,		        -- x1,y1,x2,y2,x3,y3,x4,y4
    polybool                = polybool, 		        -- p1,p2,operation
    convexpart              = convexpart                -- verts
}
