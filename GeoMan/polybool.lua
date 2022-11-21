local path = string.gsub(..., "polybool", "", 1)
local lineCross = require(path.."lineCross")

local sign = function(x)
    return x>0 and 1 or x<0 and -1 or x
end

local push = function(tab,tab2)
    for _,v in ipairs(tab2) do
        table.insert(tab,v)
    end
end

local unshift = function(tab,tab2)
    for i,v in ipairs(tab2) do
        table.insert(tab,i,v)
    end
end

local reverse = function(tab)
    local len = #tab
    local rt = {}
    for i,v in ipairs(tab) do
        rt[len-i+1] = v
    end
    return rt
end

local copy = function(tab)
    return {unpack(tab)}
end

local distance = function(x1,y1,x2,y2)
    return math.sqrt((x1-x2)^2+(y1-y2)^2)
end

local pointContain = function(x,y,p)

    local inside, iN = false, 1
    for iC = 1, #p-1, 2 do

        iN = iN < #p-1 and iN+2 or 1
        local xc, yc = p[iC], p[iC+1]
        local xn, yn = p[iN], p[iN+1]

        if ((yn > y) ~= (yc > y)) and (x < (y - yn) * (xc - xn)/(yc - yn) + xn) then
            inside = not inside
        end

    end

    return inside

end

local polygonArea = function(polygon)

    local ax,ay = 0,0
    local bx,by = 0,0

    local area = 0
    local fx,fy = polygon[1],polygon[2]

    for i = 3, #polygon-1, 2 do
        local px,py = polygon[i-2],polygon[i-1]
        local cx,cy = polygon[i],polygon[i+1]
        ax = fx - cx
        ay = fy - cy
        bx = fx - px
        by = fy - py
        area = area + (ax*by) - (ay*bx)
    end

    return area/2

end

local Node = {}
function Node:new(x,y,alpha,intersection)
    local new = {
        x = x,
        y = y,
        next = nil,
        prev = nil,
        nextPoly =nil,
        neighbor = nil,
        intersect = intersection,
        entry = nil,
        visited = false,
        alpha = alpha or 0
    }
    setmetatable(new, self)
    self.__index = self
    return new
end

function Node:nextNonIntersection()
    local a = self
    while a and a.intersect do
        a = a.next
    end
    return a
end

function Node:last()
    local a = self

    while a.next and a.next~=self do
        a = a.next
    end
    return a
end

function Node:createLoop()
    local last = self:last()
    last.prev.next = self
    self.prev = last.prev
end

function Node:firstNodeOfIntersect()
    local a = self

    while true do
        a = a.next
        if not a then break end --should check error
        if a == self then break end
        if a.intersect and not a.visited then break end
    end

    return a
end

function Node:insertBetween(first,last)
    local a = first

    while a~=last and a.alpha<self.alpha do
        a = a.next
    end

    self.next = a
    self.prev = a.prev

    if self.prev  then
        self.prev.next = self
    end

    self.next.prev = self;
end

local function cleanList(verts)
    for i = #verts , 4 , -1 do
        if verts[i-3]== verts[i-1] and
            verts[i-2] == verts[i] then
            table.remove(verts, i)
            table.remove(verts,i-1)
        end
    end
    return verts
end

local function createList(verts)
    local first,current

    for i = 1, #verts-1 , 2 do
        if current then
            current.next = Node:new(verts[i],verts[i+1])
            current.next.prev = current
            current = current.next
        else
            current = Node:new(verts[i],verts[i+1])
            first = current
        end
    end

    local next = Node:new(first.x,first.y,1)--何意？current
    current.next = next 
    next.prev = current
    return first , current -- first and last
end


local function indentifyIntersections(subjectList, clipList)

    local found = false
    local subject = subjectList

    while subject.next do
        if not subject.intersect then
            local clip = clipList
            while clip.next do
                if not clip.intersect then
                    local subjectNext = subject.next:nextNonIntersection()
                    local clipNext = clip.next:nextNonIntersection()
                    local ax,ay = subject.x , subject.y
                    local bx,by = subjectNext.x , subjectNext.y
                    local cx,cy = clip.x , clip.y
                    local dx,dy = clipNext.x, clipNext.y

                    local x,y = lineCross(ax,ay,bx,by,cx,cy,dx,dy)

                    if x and x~=true then
                        found = true
                        local alphaS = distance(ax,ay,x,y)/distance(ax,ay,bx,by)
                        local alphaC = distance(cx,cy,x,y)/distance(cx,cy,dx,dy)

                        local subjectInter = Node:new(x,y,alphaS,true)
                        local clipInter = Node:new(x,y,alphaC,true)
                        subjectInter.neighbor = clipInter
                        clipInter.neighbor = subjectInter						
                        subjectInter:insertBetween(subject,subjectNext)
                        clipInter:insertBetween(clip,clipNext)

                    end
                end
                clip = clip.next
            end

        end
        subject = subject.next
    end
    return found
end

local function indentifyIntersectionType(subjectList, clipList, clipPoly, subjectPoly, type)

    local se = pointContain(subjectList.x,subjectList.y,clipPoly)

    if type == "and" then se = not se end

    local subject = subjectList
    while subject do
        if subject.intersect then
            subject.entry = se
            se = not se
        end
        subject = subject.next
    end

    local ce = not pointContain(clipList.x,clipList.y,subjectPoly)
    if (type == "or") then ce = not ce end

    local clip = clipList
    while clip do
        if clip.intersect then
            clip.entry = ce
            ce = not ce
        end
        clip = clip.next
    end

end


local function collectClipResults(subjectList, clipList)

    subjectList:createLoop()
    clipList:createLoop()

    local results, walker = {}

    while true do

        walker = subjectList:firstNodeOfIntersect()
        if walker == subjectList then break end

        local result = {}

        while true do

            if walker.visited  then break end

            walker.visited = true
            walker = walker.neighbor
            table.insert(result,walker.x)
            table.insert(result,walker.y)
            local forward = walker.entry

            while true do

                walker.visited = true
                walker = forward and walker.next or walker.prev

                if walker.intersect then
                    break
                else
                    table.insert(result,walker.x)
                    table.insert(result,walker.y)
                end

            end

        end

        table.insert(results,cleanList(result))

    end

    return results

end


local function polygonBoolean(subjectPoly, clipPoly, operation)

    local res

    local subjectList = createList(subjectPoly)
    local clipList = createList(clipPoly)

    if indentifyIntersections(subjectList, clipList) then

        indentifyIntersectionType(
            subjectList, clipList,
            clipPoly, subjectPoly,
            operation
        )

        res = collectClipResults(subjectList, clipList)

    else

        local inner = pointContain(subjectPoly[1],subjectPoly[2],clipPoly)
        local outer = pointContain(clipPoly[1],clipPoly[2],subjectPoly)

        res = {}

        if operation == "or" then

            if not inner and not outer then
                push(res,copy(subjectPoly))
                push(res,copy(clipPoly))
            elseif inner then
                push(res,copy(clipPoly))
            elseif outer then
                push(res,copy(subjectPoly))
            end

        elseif operation == "and" then

            if inner then
                push(res,copy(subjectPoly))
            elseif outer then
                push(res,copy(clipPoly))
            else
                --error("oops")
            end

        elseif operation == "not" then

            local sclone = copy(subjectPoly)
            local cclone = copy(clipPoly)

            local sarea = polygonArea(sclone)
            local carea = polygonArea(cclone)

            if sign(sarea) == sign(carea) then
                if outer then
                    cclone = reverse(cclone)
                elseif inner then
                    sclone = reverse(sclone)
                end
            end

            push(res,sclone)

            if math.abs(sarea) > math.abs(carea) then
                push(res,cclone)
            else
                unshift(res,cclone)
            end

        end

    end

    return res

end

return polygonBoolean
