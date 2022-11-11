--[[
    The following code is based on Ivan Fratric's PolyPartition library.

    Original work Copyright (C) 2011 by Ivan Fratric

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
]]

local path = ...

local TPPL_CCW = 1
local TPPL_CW = -1

local excluded_keys = {
  ['__init'] = true,
  ['__extends'] = true,
  ['__index'] = true,
  ['__class'] = true
}
local Class
Class = function(tbl)
  assert(type(tbl) == 'table', "An initialisation table must be provided for the class")
  local parent = tbl.__extends
  local base
  do
    local _tbl_0 = { }
    for k, v in pairs(parent and parent.__base or { }) do
      if not excluded_keys[k] and tbl[k] == nil then
        _tbl_0[k] = v
      end
    end
    base = _tbl_0
  end
  local c = {
    __parent = parent,
    __base = base,
    __index = tbl.__index or (parent and parent.__index),
    __init = tbl.__init or (parent and parent.__init) or function(self) end
  }
  for k, v in pairs(tbl) do
    if not excluded_keys[k] then
      base[k] = v
    end
  end
  base.__class = c
  do
    local __index = c.__index
    if __index then
      base.__index = function(t, key)
        local olditem = base[key]
        if not (olditem == nil) then
          return olditem
        end
        local item
        local _exp_0 = type(__index)
        if "table" == _exp_0 then
          item = __index[key]
        elseif "function" == _exp_0 then
          item = __index(t, key)
        end
        return item
      end
    else
      base.__index = base
    end
  end
  return setmetatable(c, {
    __call = function(self, ...)
      local __newindex = base.__newindex
      base.__newindex = nil
      local obj = setmetatable({ }, base)
      self.__init(obj, ...)
      base.__newindex = __newindex
      return obj
    end,
    __index = base
  })
end

local sqrt = math.sqrt
local Vec2
Vec2 = Class({
  __init = function(self, x, y)
    self.x = x or 0
    self.y = y or 0
  end,
  __add = function(u, v)
    return Vec2(u.x + v.x, u.y + v.y)
  end,
  __sub = function(u, v)
    return Vec2(u.x - v.x, u.y - v.y)
  end,
  __unm = function(self)
    return Vec2(-self.x, -self.y)
  end,
  __mul = function(a, b)
    if type(a) == "number" then
      return Vec2(a * b.x, a * b.y)
    elseif type(b) == "number" then
      return Vec2(b * a.x, b * a.y)
    else
      return error("attempt to multiply a vector with a non-scalar value", 2)
    end
  end,
  __div = function(self, a)
    return Vec2(self.x / a, self.y / a)
  end,
  __eq = function(u, v)
    return u.x == v.x and u.y == v.y
  end,
  __tostring = function(v)
    return "(" .. tostring(v.x) .. ", " .. tostring(v.y) .. ")"
  end,
  __index = function(self, key)
    return key == 1 and self.x or key == 2 and self.y or nil
  end,
  dot = function(u, v)
    return u.x * v.x + u.y * v.y
  end,
  wedge = function(u, v)
    return u.x * v.y - u.y * v.x
  end,
  lenS = function(v)
    return Vec2.dot(v, v)
  end,
  len = function(self)
    return sqrt(self:lenS())
  end,
  unpack = function(self)
    return self.x, self.y
  end
})

local TPPLPoly = Class({

  __init = function(self)
    return self:clear()
  end,

  clear = function(self)
    self.points = { }
    self.hole = false
  end,

  init = function(self, numpoints)
    self:clear()
    do
      local _accum_0 = { }
      local _len_0 = 1
      for i = 1, numpoints do
        _accum_0[_len_0] = Vec2()
        _len_0 = _len_0 + 1
      end
      self.points = _accum_0
    end
  end,

  triangle = function(self, p1, p2, p3)
    self:clear()
    self.points = {
      p1,
      p2,
      p3
    }
  end,

  getOrientation = function(self)
    local area = 0
    local numpoints = #self.points
    for i = 1, #numpoints do
      local j = i == numpoints and 1 or i + 1
      area = area + (self.points[i].x * self.points[j].y - self.points[i].y * self.points[j].x)
    end
    if area > 0 then
      return TPPL_CCW
    elseif area < 0 then
      return TPPL_CW
    else
      return 0
    end
  end,

  setOrientation = function(self, orientation)
    if self:getOrientation() ~= orientation then
      return self:invert()
    end
  end,

  invert = function(self)
    local n = #self.points
    for i = 1, math.floor(n / 2) do
      self.points[i], self.points[n - i + 1] = self.points[n - i + 1], self.points[i]
    end
  end,

  setHole = function(self, flag)
    self.hole = flag
  end,

  getNumPoints = function(self)
    return #self.points
  end,

  isHole = function(self)
    return self.hole
  end,

  __index = function(self, key)
    return self.points[key]
  end,

  __newindex = function(self, key, value)
    if type(key) == "number" then
      self.points[key] = value
    else
      return rawset(self, key, value)
    end
  end,
  valid = function(self)
    return #self.points > 0
  end

})

local normalize = function(p)
  local norm = p:len()
  return norm ~= 0 and p / norm or Vec2()
end

local isConvex = function(p1, p2, p3)
  local tmp = (p3.y - p1.y) * (p2.x - p1.x) - (p3.x - p1.x) * (p2.y - p1.y)
  return tmp > 0
end

local isReflex = function(p1, p2, p3)
  local tmp = (p3.y - p1.y) * (p2.x - p1.x) - (p3.x - p1.x) * (p2.y - p1.y)
  return tmp < 0
end

local inCone = function(p1, p2, p3, p)
  if isConvex(p1, p2, p3) then
    return isConvex(p1, p2, p) and isConvex(p2, p3, p)
  else
    return isConvex(p1, p2, p) or isConvex(p2, p3, p)
  end
end

local intersects = function(p11, p12, p21, p22)
  if p11 == p21 or p11 == p22 or p12 == p21 or p12 == p22 then
    return false
  end
  local v1ort = Vec2(p12.y - p11.y, p11.x - p12.x)
  local v2ort = Vec2(p22.y - p21.y, p21.x - p22.x)
  local dot21 = Vec2.dot(p21 - p11, v1ort)
  local dot22 = Vec2.dot(p22 - p11, v1ort)
  local dot11 = Vec2.dot(p11 - p21, v2ort)
  local dot12 = Vec2.dot(p12 - p21, v2ort)
  return dot11 * dot12 <= 0 and dot21 * dot22 <= 0
end

local removeHoles = function(polys)

  local hasholes = false
  for _index_0 = 1, #polys do
    local p = polys[_index_0]
    if p.hole then
      hasholes = true
      break
    end
  end

  if not hasholes then
    return polys
  end

  local holepoly, holepointindex, bestpolypoint
  local polypointindex, poly

  while true do
  
    hasholes = false
    for _index_0 = 1, #polys do
      local _continue_0 = false

      repeat
        local p = polys[_index_0]
        if not p.hole or p.skip then
          _continue_0 = true
          break
        end

        if not hasholes then
          hasholes = true
          holepoly = p
          holepointindex = 1
        end

        for i = 1, p:getNumPoints() do
          if p[i].x > holepoly[holepointindex].x then
            holepoly = p
            holepointindex = i
          end
        end

        _continue_0 = true

      until true

      if not _continue_0 then
        break
      end

    end

    if not hasholes then
      break
    end

    local holepoint = holepoly[holepointindex]
    local pointfound = false

    for _index_0 = 1, #polys do
      local _continue_0 = false

      repeat

        local p = polys[_index_0]
        if p.hole or p.skip then
          _continue_0 = true
          break
        end

        local n = p:getNumPoints()

        for i = 1, n do
          local _continue_1 = false

          repeat
            if p[i].x <= holepoint.x then
              _continue_1 = true
              break
            end

            if not inCone(p[i > 1 and i - 1 or n], p[i], p[i < n and i + 1 or 1], holepoint) then
              _continue_1 = true
              break
            end

            local polypoint = p[i]
            if pointfound then
              local v1 = normalize(polypoint - holepoint)
              local v2 = normalize(bestpolypoint - holepoint)
              if v2.x > v1.x then
                _continue_1 = true
                break
              end
            end

            local pointvisible = true
            for _index_1 = 1, #polys do
              local _continue_2 = false

              repeat
                local p2 = polys[_index_1]
                if p2.hole then
                  _continue_2 = true
                  break
                end

                local m = p2:getNumPoints()

                for j = 1, m do
                  local linep1 = p2[j]
                  local linep2 = p2[j < m and j + 1 or 1]
                  if intersects(holepoint, polypoint, linep1, linep2) then
                    pointvisible = false
                    break
                  end
                end

                if not pointvisible then
                  break
                end

                _continue_2 = true

              until true

              if not _continue_2 then
                break
              end

            end

            if pointvisible then
              pointfound = true
              bestpolypoint = polypoint
              poly = p
              polypointindex = i
            end

            _continue_1 = true

          until true

          if not _continue_1 then
            break
          end

        end

        _continue_0 = true

      until true

      if not _continue_0 then
        break
      end

    end

    if not pointfound then
      return false
    end

    local newpoly = TPPLPoly()
    newpoly:init(holepoly:getNumPoints() + poly:getNumPoints() + 2)

    local n = 1
    for i = 1, polypointindex do
      newpoly[n] = poly[i]
      n = n + 1
    end

    local holesize = holepoly:getNumPoints()
    for i = 0, holesize do
      local k = holepointindex + i
      k = k > holesize and k - holesize or k
      newpoly[n] = holepoly[k]
      n = n + 1
    end

    for i = polypointindex, poly:getNumPoints() do
      newpoly[n] = poly[i]
      n = n + 1
    end

    polys[#polys + 1] = newpoly
    holepoly.skip = true
    poly.skip = true

  end

  local outpolys = { }
  for _index_0 = 1, #polys do
    local p = polys[_index_0]
    if not p.skip then
      outpolys[#outpolys + 1] = p
    end
  end

  return outpolys

end


local isInside = function(p1, p2, p3, p)
  if isConvex(p1, p, p2) then
    return false
  end
  if isConvex(p2, p, p3) then
    return false
  end
  if isConvex(p3, p, p1) then
    return false
  end
  return true
end

local updateVertex = function(v, vertices)
  local v1, v3 = v.previous, v.next
  v.isConvex = isConvex(v1.p, v.p, v3.p)
  local vec1 = normalize(v1.p - v.p)
  local vec3 = normalize(v3.p - v.p)
  v.angle = Vec2.dot(vec1, vec3)

  if v.isConvex then
    v.isEar = true

    for i = 1, #vertices do
      local _continue_0 = false

      repeat
        local w = vertices[i]

        if w.p == v.p or w.p == v1.p or w.p == v3.p then
          _continue_0 = true
          break
        end

        if isInside(v1.p, v.p, v3.p, w.p) then
          v.isEar = false
          break
        end

        _continue_0 = true

      until true

      if not _continue_0 then
        break
      end

    end

  else
    v.isEar = false
  end

end

local triangulate_EC
triangulate_EC = function(tbl)

  if not tbl.__class then
    local polys = removeHoles(tbl)
    local result = { }

    if not polys then
      return nil
    end

    for _index_0 = 1, #polys do
      local poly = polys[_index_0]
      local triangles = triangulate_EC(poly)
      if not triangles then
        return nil
      end
      for _index_1 = 1, #triangles do
        local triangle = triangles[_index_1]
        result[#result + 1] = triangle
      end
    end

    return result

  else
    local poly = tbl
    local numvertices = poly:getNumPoints()

    if not poly:valid() or numvertices < 3 then
      return nil
    end

    if numvertices == 3 then
      return { poly }
    end

    local vertices

    do
      local _accum_0 = { }
      local _len_0 = 1
      for i = 1, numvertices do
        _accum_0[_len_0] = { }
        _len_0 = _len_0 + 1
      end
      vertices = _accum_0
    end

    for i = 1, numvertices do
      local v = vertices[i]
      v.isActive = true
      v.p = poly[i]
      v.next = vertices[i == numvertices and 1 or i + 1]
      v.previous = vertices[i == 1 and numvertices or i - 1]
    end

    for i = 1, numvertices do
      updateVertex(vertices[i], vertices)
    end

    local ear
    local triangles = { }
    for i = 1, numvertices - 3 do
      local earfound = false

      for j = 1, numvertices do
        local _continue_0 = false

        repeat
          local v = vertices[j]

          if not v.isActive then
            _continue_0 = true
            break
          end

          if not v.isEar then
            _continue_0 = true
            break
          end

          if not earfound then
            earfound = true
            ear = v
          elseif v.angle > ear.angle then
            ear = v
          end

          _continue_0 = true

        until true

        if not _continue_0 then
          break
        end

      end

      if not earfound then
        return nil
      end

      local triangle = TPPLPoly()
      triangle:triangle(ear.previous.p, ear.p, ear.next.p)
      triangles[#triangles + 1] = triangle
      ear.isActive = false
      ear.previous.next = ear.next
      ear.next.previous = ear.previous

      if i == numvertices - 3 then
        break
      end

      updateVertex(ear.previous, vertices)
      updateVertex(ear.next, vertices)

    end

    for i = 1, numvertices do
      local v = vertices[i]
      if v.isActive then
        local triangle = TPPLPoly()
        triangle:triangle(v.previous.p, v.p, v.next.p)
        triangles[#triangles + 1] = triangle
        break
      end
    end

    return triangles

  end
end


local convexPartition_HM, list_to_lua
convexPartition_HM = function(tbl)

  if not tbl.__class then

    local polys = removeHoles(tbl)
    local result = { }

    if not polys then return nil end

    for _index_0 = 1, #polys do
      local poly = polys[_index_0]
      local parts = convexPartition_HM(poly)
      if not parts then return nil end
      for _index_1 = 1, #parts do
        local part = parts[_index_1]
        result[#result + 1] = part
      end
    end

    return result

  else

    local poly = tbl

    if not poly:valid() then return nil end

    local parts = { }
    local numreflex = 0
    local numvertices = poly:getNumPoints()

    for i11 = 1, numvertices do
      local i12 = i11 == 1 and numvertices or i11 - 1
      local i13 = i11 == numvertices and 1 or i11 + 1
      if isReflex(poly[i12], poly[i11], poly[i13]) then
        numreflex = 1
        break
      end
    end

    if numreflex == 0 then
      return { poly }
    end

    local triangles = triangulate_EC(poly)
    if not triangles then return nil end

    local poly1, num1, poly2, num2
    local i11, i12, i21, i22

    for idx1 = 1, #triangles do
      local _continue_0 = false

      repeat
        poly1 = triangles[idx1]
        if poly1.skip then
          _continue_0 = true
          break
        end

        num1 = poly1:getNumPoints()
        i11 = 0

        while i11 < num1 do
          local _continue_1 = false

          repeat
            i11 = i11 + 1
            local d1 = poly1[i11]
            i12 = i11 == num1 and 1 or i11 + 1
            local d2 = poly1[i12]
            local isdiagonal = false

            for _index_0 = 1, #triangles do
              local _continue_1 = false

              repeat
                local p = triangles[_index_0]

                if p.skip or p == poly1 then
                  _continue_1 = true
                  break
                end

                poly2 = p
                num2 = poly2:getNumPoints()

                for i = 1, num2 do
                  local _continue_2 = false

                  repeat

                    do
                      i21 = i
                      if d2 ~= poly2[i21] then
                        _continue_2 = true
                        break
                      end
                      i22 = i21 == num2 and 1 or i21 + 1
                      if d1 ~= poly2[i22] then
                        _continue_2 = true
                        break
                      end
                      isdiagonal = true
                      break
                    end

                    _continue_2 = true

                  until true

                  if not _continue_2 then
                    break
                  end

                end

                if isdiagonal then break end
                _continue_1 = true

              until true
              if not _continue_1 then break end

            end

            if not isdiagonal then
              _continue_1 = true
              break
            end

            local p2 = poly1[i11]
            local i13 = i11 == 1 and num1 or i11 - 1
            local p1 = poly1[i13]
            local i23 = i22 == num2 and 1 or i22 + 1
            local p3 = poly2[i23]

            if not isConvex(p1, p2, p3) then
              _continue_1 = true
              break
            end

            p2 = poly1[i12]
            i13 = i12 == num1 and 1 or i12 + 1
            p3 = poly1[i13]
            i23 = i21 == 1 and num2 or i21 - 1
            p1 = poly2[i23]

            if not isConvex(p1, p2, p3) then
              _continue_1 = true
              break
            end

            local newpoly = TPPLPoly()
            newpoly:init(num1 + num2 - 2)
            local k = 1
            local j = i12

            while j ~= i11 do
              newpoly[k] = poly1[j]
              k = k + 1
              j = j == num1 and 1 or j + 1
            end

            j = i22

            while j ~= i21 do
              newpoly[k] = poly2[j]
              k = k + 1
              j = j == num2 and 1 or j + 1
            end

            triangles[idx1] = newpoly
            poly1 = newpoly
            num1 = newpoly:getNumPoints()
            poly2.skip = true
            i11 = 0
            _continue_1 = true

          until true

          if not _continue_1 then break end

        end

        _continue_0 = true

      until true

      if not _continue_0 then
        break
      end

    end

    for _index_0 = 1, #triangles do
      local poly = triangles[_index_0]
      if not poly.skip then
        parts[#parts + 1] = poly
      end
    end

    return parts

  end
end

list_to_lua = function(polys)
  local result = { }
  local _list_0 = (polys or { })
  for _index_0 = 1, #_list_0 do
    local poly = _list_0[_index_0]
    local vertices = { }
    for i = 1, poly:getNumPoints() do
      local v = poly[i]
      table.insert(vertices, v.x)
      table.insert(vertices, v.y)
      --[[
      vertices[i] = {
        v.x,
        v.y
      }
      ]]
    end
    result[#result + 1] = vertices
  end
  return result
end


local newTPPLPoly = function(poly)

  local cPoly = TPPLPoly()
  cPoly:init(#poly/2)

  local i = 1
  for j = 1, #poly-1, 2 do
    cPoly.points[i].x = poly[j]
    cPoly.points[i].y = poly[j+1]
    i = i + 1
  end

  return cPoly

end

local convertTPPLPoly = function(poly)
  if poly == nil then return nil end

  local cPoly = { }

  if not poly.__class then -- table of polygons

    for i = 1, #poly do
      cPoly[i] = { }
      for j = 1, poly[i]:getNumPoints() do
        table.insert(cPoly[i], poly[i].points[j].x)
        table.insert(cPoly[i], poly[i].points[j].y)
      end
    end

  else 

    for i = 1, poly:getNumPoints() do
      table.insert(cPoly, poly.points[i].x)
      table.insert(cPoly, poly.points[i].x)
    end

  end

  return cPoly

end

local convexPartAuto = function(poly)

  if type(poly[1]) == "table" then
    for i = 1, #poly do
      local pl = poly[i]
      pl = newTPPLPoly(pl)
      pl = convexPartition_HM(pl)
      pl = convertTPPLPoly(pl)
    end
  else
    poly = newTPPLPoly(poly)
    poly = convexPartition_HM(poly)
    poly = convertTPPLPoly(poly)
  end

  return poly

end

--[[
return {
  TPPLPoly        = TPPLPoly,
  new             = newTPPLPoly,
  convert         = convertTPPLPoly,
  list_to_lua     = list_to_lua,
  convexPart      = convexPartition_HM,
  convexPartAuto  = convexPartAuto
}]]

return convexPartAuto