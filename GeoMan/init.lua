local path = ...

local Delaunay = require(path.."/delaunay")

local polybool   = require(path.."/polybool")
local convexpart = require(path.."/convexpart")
local lineCross  = require(path.."/lineCross")



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

local cmp = function (x1,y1,x2,y2)
    return x1 == x2 and y1 == y2
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


local function keepMidPoints(points, keepOlds)

    local pts = {}

    for i = 1, #points-1, 2 do

        local j = i+2 < #points and i+2 or 1

        local x = (points[i] + points[j]) / 2
        local y = (points[i+1] + points[j+1]) / 2

        if keepOlds then
            pts[#pts+1] = points[i]
            pts[#pts+1] = points[i+1]
        end

        pts[#pts+1], pts[#pts+2] = x, y

    end

    return pts

end


local function getMiddle(x1,y1,x2,y2)
    return (x1+x2)/2, (y1+y2)/2
end


local function getDist(x1,y1,x2,y2)
    return math.sqrt((x1-x2)^2+(y1-y2)^2)
end


local function getSegDist(x,y, x1,y1, x2,y2)

    local dx = x2 - x1
    local dy = y2 - y1

    if dx ~= 0 or dy ~= 0 then

        local t = ((x - x1) * dx + (y - y1) * dy) / (dx^2 + dy^2)

        if t > 1 then
            x1, y1 = x2, y2
        elseif t > 0 then
            x1 = x1 + dx * t
            y1 = y1 + dy * t
        end

    end

    dx = x - x1
    dy = y - y1

    return dx^2 + dy^2

end

local simplifyRadialDistance = function(points, sqTolerance)

    local prev_x, x = points[1]
    local prev_y, y = points[2]

    local new_points = {prev_x, prev_y}

    for i = 1, #points-1, 2 do

        x, y = points[i], points[i+1]

        if getDist(x,y, prev_x,prev_y) > sqTolerance then
            new_points[#new_points+1] = x
            new_points[#new_points+1] = y
            prev_x, prev_y = x, y
        end

    end

    if prev_x ~= x and prev_y ~= y then
        new_points[#new_points+1] = x
        new_points[#new_points+1] = y
    end

    return new_points

end

local simplifyDPStep
simplifyDPStep = function(points, first, last, sqTolerance, simplified)

    local maxDist, index = sqTolerance

    for i = first+2, last, 2 do

        local dist = getSegDist(
            points[i], points[i+1],
            points[first], points[first+1],
            points[last], points[last+1]
        )

        if (dist > maxDist) then
            index, maxDist = i, dist
        end

    end

    if maxDist > sqTolerance then

        if index - first > 1 then
            simplifyDPStep(points, first, index, sqTolerance, simplified)
            simplified[#simplified+1] = points[index]
            simplified[#simplified+1] = points[index+1]
        end

        if last - index > 1 then
            simplifyDPStep(points, index, last, sqTolerance, simplified)
        end

    end

end

local simplifyDouglasPeucker = function(points, sqTolerance)

    local last = #points-1
    local simplified = {points[1], points[2]}

    simplifyDPStep(points, 1, last, sqTolerance, simplified)

    simplified[#simplified+1] = points[last]
    simplified[#simplified+1] = points[last+1]

    return simplified;

end

local function simplifyPoly(points, tolerance, highestQuality)

    tolerance = tolerance or .1
    highestQuality = highestQuality or true

    local sqtolerance = tolerance ^ 2

    if not highestQuality then
        points = simplifyRadialDistance(points, sqtolerance)
    end

    points = simplifyDouglasPeucker(points, sqtolerance)

    return points

end


local function newPolygonTrans(p,x,y,rot,size)

    rot, size = rot or 0, size or 1

    local tab={}
    for i=1,#p/2 do
        tab[2*i-1],tab[2*i]=axisRot(p[2*i-1],p[2*i],rot)
        tab[2*i-1]=tab[2*i-1]*size+x
        tab[2*i]=tab[2*i]*size+y
    end

    return tab

end


local function polygonTrans(p,x,y,rot,size)

    rot, size = rot or 0, size or 1

    for i=1,#p/2 do
        p[2*i-1], p[2*i] = axisRot(p[2*i-1], p[2*i], rot)
        p[2*i-1] = p[2*i-1]*size+x
        p[2*i] = p[2*i]*size+y
    end

end


local function getRot(x1,y1,x2,y2,toggle)
    if x1==x2 and y1==y2 then return 0 end
    local angle=math.atan((x1-x2)/(y1-y2))
    if y1-y2<0 then angle=angle-math.pi end
    if toggle==true then angle=angle+math.pi end
    if angle>0 then angle=angle-2*math.pi end
    if angle==0 then return 0 end
    return -angle
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


local function isCircleInCircle(x1,y1,r1, x2,y2,r2, repos)

    if repos then

        local dx = x1 - x2
        local dy = y1 - y2
        local rr = r1 + r2

        local dist = math.sqrt(dx^2+dy^2)

        local overlap = (dist - rr)

        local x = x1 - (dx / dist) * overlap
        local y = y1 - (dy / dist) * overlap

        if dist <= rr then
            return true, x, y
        end

    elseif getDist(x1,y1, x2,y2) <= r1+r2 then
        return true
    end

    return false

end


local function isCircleOutCircle(x1,y1,r1, x2,y2,r2, repos)

    if r1 < r2 then  r1, r2 = r2, r1 end

    if repos then

        local dx = x1 - x2
        local dy = y1 - y2
        local rr = r1 - r2

        local dist = math.sqrt(dx^2+dy^2)

        local overlap = (dist - rr)

        local x = x1 - (dx / dist) * overlap
        local y = y1 - (dy / dist) * overlap

        if dist >= rr then
            return true, x, y
        end

    elseif getDist(x1,y1, x2,y2) >= r1-r2 then
        return true
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

        local uc = ((y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1))
        local ua = (((x4 - x3) * (y1 - y3)) - (y4 - y3) * (x1 - x3)) / uc
        local ub = (((x2 - x1) * (y1 - y3)) - ((y2 - y1) * (x1 - x3))) / uc

        if ua >= 0 and ua <= 1 and ub >= 0 and ub <= 1 then
            return px, py
        else
            return false
        end

    end

    return px, py

end


local function nearestPoint(x,y,points)

    local shortest_dist, nx, ny = math.huge

    for i = 1, #points-1, 2 do
        local px, py = points[i], points[i+1]
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
    return newPolygonTrans(convexHull(v),x,y,0,1)
end


--return area, center
local function getPolyArea(verts)

    local area = 0
    local cx,cy=0,0

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


local function setPolyPosition(p,x,y,isCenter)  -- TODO: must be optimized

    local px,py, _

    if isCenter then _,px,py = getPolyArea(p)
    else _,_,px,py = getPolyDimensions(p) end

    local dx = px - x
    local dy = py - y

    for i = 1, #p-1, 2 do
        p[i] = p[i] - dx
        p[i+1] = p[i+1] - dy
    end

end


local function getPolyLength(poly, isLine)

    local len = 0

    if isLine then
        for i = 1, #poly-3, 2 do
            len = len + getDist(
                poly[i], poly[i+1],
                poly[i+2], poly[i+3]
            )
        end
    else
        for i = 1, #poly-1, 2 do
            local j = i+2 < #poly and i+2 or 1
            len = len + getDist(
                poly[i], poly[i+1],
                poly[j], poly[j+1]
            )
        end
    end

    return len

end


local function isPolysIntersect_AABB(p1, p2)

    local _,_, xMin1,yMin1, xMax1,yMax1 = getPolyDimensions(p1)
    local _,_, xMin2,yMin2, xMax2,yMax2 = getPolyDimensions(p2)

    return not (
      xMax1 < xMin2 or yMax1 < yMin2 or
      xMin1 > xMax2 or yMin1 > yMax2
    )

end


local function isPolysIntersect(p1, p2)

    for i1 = 1, #p1-1, 2 do

        local i2 = i1+2 < #p1 and i1+2 or 1

        for j1 = 1, #p2-1, 2 do

            local j2 = j1+2 < #p2 and j1+2 or 1

            if linesIntersect(
                p1[i1], p1[i1+1], p1[i2], p1[i2+1],
                p2[j1], p2[j1+1], p2[j2], p2[j2+1]
            ) then
                return true
            end

        end
    end

    return false

end


local function isPolyInPoly(p1, p2, entirely) -- TODO: can be optimized but good example

    local intersect = isPolysIntersect(p1, p2)

    if not entirely and intersect then
        return true
    else

        for i = 1, #p1-1, 2 do

            local inside = isPointInPoly(p1[i], p1[i+1], p2)

            if (not intersect and inside)
            or (not entirely and inside)
            then
                return true
            end

        end

    end

    return false

end


local function isPolySelfIntersect(p, filterFunc) -- TODO: Can be optimized by implementing Bentley Ottman's algorithm

    local seen = setmetatable({}, {__index = function(t,k)
        local s = {}; t[k] = s; return s
    end})

    local len = #p

    local isects = {}

    for i = 1, len-1, 2 do

        local _i = i+2 < len and i+2 or 1

        local x1, y1 = p[i], p[i+1]
        local x2, y2 = p[_i], p[_i+1]

        for j = 1, len-1, 2 do

            if i ~= j then

                local _j = j+2 < len and j+2 or 1

                local x3, y3 = p[j], p[j+1]
                local x4, y4 = p[_j], p[_j+1]

                if not (
                   cmp(x1,y1, x3,y3)
                or cmp(x2,y2, x3,y3)
                or cmp(x1,y1, x4,y4)
                or cmp(x2,y2, x4,y4)
                ) then

                    local ix,iy = linesIntersect(
                        x1,y1,x2,y2, x3,y3,x4,y4
                    )

                    if ix then

                        if not (
                           cmp(ix,iy, x1,y1)
                        or cmp(ix,iy, x2,y2)
                        or cmp(ix,iy, x3,y3)
                        or cmp(ix,iy, x4,y4)
                        ) then

                            local unique = not seen[ix][iy]

                            if unique then
                                seen[ix][iy] = true
                            end

                            local collect = unique

                            if filterFunc then
                                collect = filterFunc(ix,iy, i, x1,y1, x2,y2, j, x3,y3, x4,y4, unique)
                            end

                            if collect then
                                isects[#isects+1] = ix
                                isects[#isects+1] = iy
                            end

                        end
                    end
                end
            end
        end
    end

    return (#isects > 0) and isects or nil

end


local cross2 = function(tri1, tri2)

    local dxA = tri1[1] - tri2[5]
    local dyA = tri1[2] - tri2[6]
    local dxB = tri1[3] - tri2[5]
    local dyB = tri1[4] - tri2[6]
    local dxC = tri1[5] - tri2[5]
    local dyC = tri1[6] - tri2[6]

    local dxBA = tri2[5] - tri2[3]
    local dxAC = tri2[1] - tri2[5]
    local dyAB = tri2[4] - tri2[6]
    local dyCA = tri2[6] - tri2[2]

    local d = dyAB * (tri2[1] - tri2[5]) + dxBA * (tri2[2] - tri2[6])

    local sa = dyAB * dxA + dxBA * dyA
    local sb = dyAB * dxB + dxBA * dyB
    local sc = dyAB * dxC + dxBA * dyC

    local ta = dyCA * dxA + dxAC * dyA
    local tb = dyCA * dxB + dxAC * dyB
    local tc = dyCA * dxC + dxAC * dyC

    if d < 0 then
        return (
            (sa >= 0 and sb >= 0 and sc >= 0)
         or (ta >= 0 and tb >= 0 and tc >= 0)
         or (sa+ta <= d and sb+tb <= d and sc+tc <= d)
        )
    end

    return (
        (sa <= 0 and sb <= 0 and sc <= 0)
     or (ta <= 0 and tb <= 0 and tc <= 0)
     or (sa+ta >= d and sb+tb >= d and sc+tc >= d)
    )

end

local function isTrianglesIntersect(tri1, tri2)
  return not (cross2(tri1,tri2) or cross2(tri2,tri1))
end


local function getTriArea(tri)

    local p = getDist(tri[1],tri[2], tri[3],tri[4])
    local q = getDist(tri[3],tri[4], tri[5],tri[6])
    local r = getDist(tri[5],tri[6], tri[1],tri[2])

    local s = (p+q+r)*.5

    return (s*(s-p)(s-q)(s-r))^.5

end


local function isSegmentsOverlap(x1,y1,x2,y2, x3,y3,x4,y4) -- NEW

    local a1 = math.atan2(x1-x2, y1-y2)
    local a2 = math.atan2(x3-x4, y3-y4)

    if a1 == a2 then

        return (
            isBetween(x1,y1, x3,y3,x4,y4)
         or isBetween(x2,y2, x3,y3,x4,y4)
         or isBetween(x3,y3, x1,y1,x2,y2)
         or isBetween(x4,y4, x1,y1,x2,y2) )

    end

    return false

end


local function getPolysSharedEdges(p1,p2,round_v) -- NEW

    local ise = {}

    for i = 1, #p1-1, 2 do
        local _i = i+2 < #p1 and i+2 or 1

        local x1,y1 = p1[i], p1[i+1]
        local x2,y2 = p1[_i], p1[_i+1]

        if round_v then
            x1,y1 = round(x1),round(y1)
            x2,y2 = round(x2),round(y2)
        end

        for j = 1, #p2-1, 2 do
            local _j = j+2 < #p2 and j+2 or 1

            local x3,y3 = p1[j], p1[j+1]
            local x4,y4 = p1[_j], p1[_j+1]

            if round_v then
                x3,y3 = round(x3),round(y3)
                x3,y3 = round(x3),round(y3)
            end

            if isSegmentsOverlap(x1,y1,x2,y2, x3,y3,x4,y4) then
                ise[#ise+1] = {i,_i ,j,_j}
            end

        end

    end

    return #ise > 0 and ise or nil

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
    keepMidPoints           = keepMidPoints,            -- points, keepOlds
    getSegDist              = getSegDist,               -- x,y,x1,y1,x2,y2
    simplifyPoly            = simplifyPoly,             -- points, tolerance, highestQuality
    getPolyArea             = getPolyArea, 			    -- verts
    getPolyDimensions       = getPolyDimensions,        -- verts
    getPolyLength           = getPolyLength,            -- verts,isLine
    convexHull              = convexHull,		        -- verts
    concaveHull             = concaveHull, 		        -- verts
    translate               = polygonTrans, 	        -- x,y,rot,size,verts
    newTranslated           = newPolygonTrans,          -- x,y,rot,size,verts
    getMiddle               = getMiddle,                -- x1,y1,x2,y2
    getDist                 = getDist,                  -- x1,y1,x2,y2
    getAngle                = getRot,                   -- x1,y1,x2,y2,toggle
    setPolyPosition         = setPolyPosition,          -- points,x,y,isCenter
    isPolySelfIntersect     = isPolySelfIntersect,      -- verts
    isPolysIntersect_AABB   = isPolysIntersect_AABB,    -- verts1, verts2
    isPolysIntersect        = isPolysIntersect,         -- verts1, verts2
    isPolyInPoly            = isPolyInPoly,             -- verts1, verts2, entirely
    isPointInPoly           = isPointInPoly,	        -- x,y,verts
    isCircleInPoly          = isCircleInPoly,           -- x,y,r,verts,isOut
    isCircleOutPoly         = isCircleOutPoly,          -- x,y,r,verts
    isCircleInCircle        = isCircleInCircle,         -- x1,y1,r1,x2,y2,r2,repos
    isCircleOutCircle       = isCircleOutCircle,        -- x1,y1,r1,x2,y2,r2,repos
    isTrisIntersect         = isTrianglesIntersect,     -- tri1,tri2
    isPointInTri            = isPointInTri,             -- x,y,triangle
    getPolysSharedEdges     = getPolysSharedEdges,      -- verts1, verts2, round_v
    getTriArea              = getTriArea,               -- triangle
    getTriCenter            = getTriCenter,             -- triangle
    getAdjacentTris         = getAdjacentTris,          -- index, triangles
    isBetween               = isBetween,                -- x,y,x1,y1,x2,y2
    linesIntersect          = linesIntersect,           -- x1,y1,x2,y2,x3,y3,x4,y4
    isSegmentsOverlap       = isSegmentsOverlap,        -- x1,y1,x2,y2,x3,y3,x4,y4
    nearestPoint            = nearestPoint,             -- x,y,points
    lineCross               = lineCross,		        -- x1,y1,x2,y2,x3,y3,x4,y4
    polybool                = polybool, 		        -- p1,p2,operator
    convexpart              = convexpart                -- verts
}
