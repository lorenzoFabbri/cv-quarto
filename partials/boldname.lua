-- https://www.neilwright.uk/posts/quarto-bibliography-format-name.html

local highlight_author_filter = {
  Span = function(el)
    for k,_ in ipairs(el.content) do
          if el.content[k].t == "Str" and el.content[k].text == "Fabbri"
          and el.content[k+1].t == "Space"
          and el.content[k+2].t == "Str" and el.content[k+2].text:find("^L") then
              local _,e = el.content[k+2].text:find("^L")
              local rest = el.content[k+2].text:sub(e+1)
              el.content[k] = pandoc.Strong { pandoc.Str("Fabbri L") }
              el.content[k+1] = pandoc.Str(rest)
              table.remove(el.content, k+2)
          end
    end
    return el
  end
}

function Div (div)
  if div.identifier:find("^ref-") then
    return pandoc.walk_block(div, highlight_author_filter)
  end
  return nil
end
