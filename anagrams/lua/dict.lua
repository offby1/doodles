d1 = { a=1, b=2 }
d2 = { a=2, b=1 }
d3 = { }

function dump (t)
   res = "{ "
   table.foreach (t,
                  function (i, v)
                     res = res .. i
                     res = res .. "="
                     res = res .. v
                     res = res .. "; "
                  end
               )
   res = res .. "}\n"
   return res
end

-- gaah.  I hate that I have to write this.
function clone(t)            -- return a copy of the table t
   local new = {}             -- create a new table
   local i, v = next(t, nil)  -- i is an index of t, v = t[i]
   while i do
      new[i] = v
      table.setn (new, table.getn (new) + 1)
      i, v = next(t, i)        -- get next index
   end
   return new
end

function sub (top, bottom)
   print ("top", dump (top))
   print ("bottom", dump (bottom))
   diff = clone (top)
   table.foreach (bottom, 
                  function (i, v)
                     print ("i: ", i, 
                            "top[i]: ", top[i],
                            "bottom[i]: ", v,
                            "Diff[i]: ", diff[i])
                     if (not (top[i])) then
                        table.setn (diff, 0)
                        return 0
                     end
                     diff[i] = top[i] - v;
                     if (diff[i] <= 0) then
                        table.setn (diff, 0)
                        return 0
                     end
                     table.setn(diff, table.getn(diff) + 1)
                  end
               )
   print ("Diff", dump (diff), "n is ", table.getn (diff))
   if (0 == table.getn (diff)) then
      return Nil
   end
   return diff
end

assert (sub (d1, d3))
assert (not (sub (d1, d2)))
assert (not (sub (d2, d1)))
assert (not (sub (d3, d2)))
assert (not (sub (d3, d1)))
