d1 = { a=1, b=2 }
d2 = { a=2, b=1 }
a1 = { a=1 }
d3 = { }

function dump (t)
   if (not (t)) then return "nil" end

   res = "{ "
   table.foreach (t,
                  function (i, v)
                     res = res .. i
                     res = res .. "="
                     res = res .. v
                     res = res .. "; "
                  end
               )
   res = res .. "}"

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

   local diff = clone (top)

   function update_diff (index, top_value)
      print ("Update_diff: index is", index, "; top_value is", top_value)
      function one_diff (top, bottom)
         if (not (top)) then return Nil end
         if (not (bottom)) then return top end
         if (top < bottom) then return Nil end
         return top - bottom
      end

      local this_diff = one_diff (top_value, bottom[index])
      if (not (this_diff)) then return 0 end
      diff[index] = this_diff
   end

   if (table.foreach (top, update_diff)) then diff = Nil end

   if (diff and (0 == (table.getn (diff)))) then diff = Nil end

   print ("diff", dump (diff), "\n")
   return diff
end

assert (sub (d1, d3))
assert (not (sub (d1, d2)))
assert (not (sub (d2, d1)))
assert (not (sub (d3, d2)))
assert (not (sub (d3, d1)))
diff = sub (d1, a1)
assert (diff.b == 2) 
assert (not (diff[a]))
