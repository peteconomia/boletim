function Cite(el)
  local cite_id = el.citations[1].id

  local function tex_ref_output(item, command)
    local key = item:match("[a-zA-Z0-9|_|-]+$")
    return pandoc.RawInline("latex", "\\" .. command .. key .. "}")
  end

  if cite_id:match("abbr:") ~= nil then
    return tex_ref_output(cite_id, "abbr{")
  end

  if cite_id:match("fig:") ~= nil then
    return tex_ref_output(cite_id, "ref{fig:")
  end

  if cite_id:match("tab:") ~= nil then
    return tex_ref_output(cite_id, "ref{tab:")
  end

  if cite_id:match("eq:") ~= nil then
    return tex_ref_output(cite_id, "ref{eq:")
  end

  if cite_id:match("box:") ~= nil then
    return tex_ref_output(cite_id, "ref{")
  end

  if cite_id:match("ch:") ~= nil then
    return tex_ref_output(cite_id, "ref{")
  end

  if cite_id:match("sec:") ~= nil then
    return tex_ref_output(cite_id, "ref{")
  end

  if cite_id:match("subsec:") ~= nil then
    return tex_ref_output(cite_id, "ref{")
  end

end