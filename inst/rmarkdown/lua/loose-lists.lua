-- Force all lists to be "loose" and therefore allow application of "List Number"
-- and "List Bullet" paragraph styles

local function plain_to_para(block)
  if block.t == "Plain" then
    return pandoc.Para(block.content)
  end
  return block
end

local function loosen_items(items)
  local out = {}
  for i, item in ipairs(items) do
    local new_item = {}
    for j, block in ipairs(item) do
      new_item[j] = plain_to_para(block)
    end
    out[i] = new_item
  end
  return out
end

function BulletList(el)
  return pandoc.BulletList(loosen_items(el.content))
end

function OrderedList(el)
  return pandoc.OrderedList(loosen_items(el.content), el.listAttributes)
end