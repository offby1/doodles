d1 = { a=1, b=2 }
d2 = { a=2, b=1 }
a1 = { a=1 }
d3 = { }

letters = {}
for c =string.byte ("a"), string.byte ("z") do letters[string.char (c)] = "!" end

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

   res = res .. " size: " .. table.getn (t)
   
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

   local diff = {}

   -- counter-intuitively, a return code of Nil means success; anything else means failure.
   function update_diff (index, ignore)
      local t = top[index]
      local b = bottom[index]
      -- print ("Update_diff: index is", index, "; t is", t, ";b is", b)

      --  top       bottom          result
      --  --------------------------------
      --  nil       nil             continue
      --  nil       n               fail
      --  n         nil             n
      --  n         m>n             fail
      --  n         m<=n            n - m               

      if (not (t)) then
         if (b) then return 0 end
      else
         if (not (b)) then 
            diff[index] = t
         else 
            if (b > t) then return 0 end
            diff[index] = t - b
         end
      end
      table.setn (diff, 0)
      table.foreach (diff,
                    function (index, value) 
                    table.setn (diff, table.getn (diff) + value)
                    end)
   end

   -- TODO -- it might make more sense to examine just the union of the letters
   -- that appear as keys in both tables, rather than examining all 26 letters
   -- all the time.
   if (table.foreach (letters, update_diff)) then diff = Nil end

   print ("diff", dump (diff), "\n")
   return diff
end

function bag_empty (b)
   return (0 == table.getn (b))
end

assert (sub (d1, d3))
assert (not (sub (d1, d2)))
assert (not (sub (d2, d1)))
assert (not (sub (d3, d2)))
assert (not (sub (d3, d1)))
diff = sub (d1, a1)
assert (diff.b == 2) 
assert (not (diff[a]))
should_be_empty = (sub (d1, d1))
assert (should_be_empty)
assert (bag_empty (should_be_empty))

function is_lc_char (c)
   return ((string.byte (c) <= string.byte ("z"))
          and
          (string.byte (c) >= string.byte ("a"))
          )
end

function from_string (s)
   local bag = {}
   local s = string.lower (s)
   while (string.len (s) > 0) do
     local c = string.sub (s, 0, 1)
     local orig = bag [c] 
     if (not (orig)) then orig = 0 end
     if (is_lc_char (c)) then bag [c] = orig + 1 end
     s = string.sub (s, 2)  
   end 
   return bag
end

b = from_string ("Hey you!")
assert (not (b.H))
assert (1 == b.h)
assert (2 == b.y)
assert (not( b[" "]))
assert (not( b["!"]))
