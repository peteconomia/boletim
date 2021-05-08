local refs = {
  acrs = {cmd = '\\acrshort{%s}'},
  acrf = {cmd = '\\acrfull{%s}'},
  acrl = {cmd = '\\acrlong{%s}'},
  abbr = {cmd = '\\acrshort{%s}'},
  abbrf = {cmd = '\\acrfull{%s}'},
  abbrl = {cmd = '\\acrlong{%s}'},
  fig = {cmd = '\\ref{fig:%s}'},
  tab = {cmd = '\\ref{tab:%s}'},
  eq = {cmd = '\\ref{eq:%s}'},
  ch = {cmd = '\\ref{ch:%s}'},
  sec = {cmd = '\\ref{sec:%s}}'},
  subsec = {cmd = '\\ref{subsec:%s}'},
  lst = {cmd = '\\ref{lst:%s}'},
  itm = {cmd = '\\ref{itm:%s}'},
  alg = {cmd = '\\ref{alg:%s}'},
  appendix = {cmd = '\\ref{appendix:%s}'},
  page = {cmd = '\\pageref{%s}'},
  name = {cmd = '\\nameref{%s}'}
}


local function latex_ref_output(latex_command, id)
  return string.format(refs[latex_command].cmd, id)
end

local function latex_commands(prefix)
  local first_command = prefix:match('[a-zA-Z]+') or ""

  local function get_secondary_command()
    local check = prefix:match(':[a-zA-Z]+:')
    if check ~= nil then
      return check:match('[a-z]+:') or ""
    end
    return ""
  end

  return first_command, get_secondary_command()

end

function CrossRef(el)
  local citation = el.citations[1]
  
  if citation.mode ~= "NormalCitation" and citation.prefix[1].text == nil then return end

  local first, second = latex_commands(citation.prefix[1].text)

  if refs[first] ~= nil then
    return pandoc.RawInline('latex', latex_ref_output(first, second .. citation.id))
  end
end

return {
  {Cite = CrossRef}
}