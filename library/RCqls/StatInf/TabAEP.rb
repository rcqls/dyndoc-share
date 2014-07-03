module CqlsProba

def CqlsProba.pretty_num(val)
    if val.to_i==val.to_f
      val.to_i.to_s
    else
      val.to_f.to_s
    end
end

#parseLoiProba(e) avec e de la forme p2, p[2 , p2[, p[2,3[, q0.95, q95%, mean, sd, var
def CqlsProba.parseFormula(e)
  case e.strip
  when "mean","m","moy","moyenne"
      {:type=>:m}
  when "var","v","sigma2","variance"
      {:type=>:v}
  when "sd","standard deviation","ecart-type"
      {:type=>:s}
  when /^q(\-?\d*\.?\d*)(\%?)$/
      ordre= "("+$1+").to_f"+($2=="%" ? "/100" : "" )
      ordrePercent=CqlsProba.pretty_num(eval(ordre+"*100"))
      {:type=>:q,:order=>ordre,:orderPercent=>ordrePercent}
  when /^p([\[\]]?)(\-?\d*\.?\d*)(;?)(\-?\d*\.?\d*)([\[\]]?)$/
      {:type=>:p,:open=>$1,:binf=>$2,:sep=>$3,:bsup=>$4,:close=>$5}
  end
end


def CqlsProba.aepR(e,opt={:y=>".yyAEP"})
  case e[:type]
  when :m
    "mean(#{opt[:y]})"    
  when :v
    "var(#{opt[:y]})"
  when :s
    "sd(#{opt[:y]})"
  when :q
    "quantile(#{opt[:y]},"+eval(e[:order]).to_s+")"
  when :p
    instR="mean("
    # égalité
    if e[:open]+e[:sep]+e[:bsup]+e[:close]=="" and e[:binf]!=""
      instR << "#{opt[:y]}=="+e[:binf]
    else
      if "[]".include? e[:open] and e[:binf]!="" and e[:sep]+e[:bsup]+e[:close]=="" 
        instR << e[:binf]+(e[:open]=="[" ? " <= " : " < ")+"#{opt[:y]}"
      elsif "[]".include? e[:close] and e[:binf]!="" and e[:sep]+e[:open]+e[:bsup]=="" 
        instR << "#{opt[:y]}"+(e[:close]=="[" ? " < " : " <= ")+e[:binf]
      elsif "[]".include? e[:open] and e[:binf]!="" and e[:sep]==";" and "[]".include? e[:close] and e[:bsup]!=""
        instR << "("+e[:binf]+(e[:open]=="[" ? " <= " : " < ")+"#{opt[:y]}) & (#{opt[:y]}"+(e[:close]=="[" ? " < " : " <= ")+e[:bsup]+")"
      end
    end
    instR << ")"
    instR
  end
end

def CqlsProba.aepTex(e,opt={:y=>"y",:m=>"m"})
  case e[:type]
  when :m
    "\\meanEmp[#{opt[:m]}]{#{opt[:y]}}"      
  when :v
    "\\Big(\\sdEmp[#{opt[:m]}]{#{opt[:y]}}\\Big)^2"
  when :s
    "\\sdEmp[#{opt[:m]}]{#{opt[:y]}}"
  when :q
    "\\quantEmp[#{opt[:m]}]{#{opt[:y]}}{"+e[:orderPercent]+"\\%}"
  when :p
    instAEP="\\meanEmp[#{opt[:m]}]{"
    # égalité
    if e[:open]+e[:sep]+e[:bsup]+e[:close]=="" and e[:binf]!=""
      instAEP << "#{opt[:y]}="+e[:binf]
    else
      if "[]".include? e[:open] and e[:binf]!="" and e[:sep]+e[:bsup]+e[:close]=="" 
        instAEP << "#{opt[:y]}"+(e[:open]=="[" ? "\\geq" : ">")+e[:binf]
      elsif "[]".include? e[:close] and e[:binf]!="" and e[:sep]+e[:open]+e[:bsup]=="" 
        instAEP << "#{opt[:y]}"+(e[:close]=="[" ? "<" : "\\leq")+e[:binf]
      elsif "[]".include? e[:open] and e[:binf]!="" and e[:sep]==";" and "[]".include? e[:close] and e[:bsup]!=""
        instAEP << "#{opt[:y]}\\in "+e[:open]+e[:binf]+","+e[:bsup]+e[:close]
      end
    end
    instAEP << "}"
    instAEP
  end
end


def CqlsProba.ampTex(e,opt={:y=>"Y"})
  case e[:type]
  when :m
    "\\EEE{#{opt[:y]}}"    
  when :v
    "\\VVV{#{opt[:y]}}"
  when :s
    "\\sigma(#{opt[:y]})"
  when :q
    "\\quant{#{opt[:y]}}{"+e[:orderPercent]+"\\%}"
  when :p
    instAMP="\\PPP{"
    # égalité
    if e[:open]+e[:sep]+e[:bsup]+e[:close]=="" and e[:binf]!=""
      instAMP << "#{opt[:y]}="+e[:binf]
    else
      if "[]".include? e[:open] and e[:binf]!="" and e[:sep]+e[:bsup]+e[:close]=="" 
        instAMP << "#{opt[:y]}"+(e[:open]=="[" ? "\\geq" : ">")+e[:binf]
      elsif "[]".include? e[:close] and e[:binf]!="" and e[:sep]+e[:open]+e[:bsup]=="" 
        instAMP << "#{opt[:y]}"+(e[:close]=="[" ? "<" : "\\leq")+e[:binf]
      elsif "[]".include? e[:open] and e[:binf]!="" and e[:sep]==";" and "[]".include? e[:close] and e[:bsup]!=""
        instAMP << "#{opt[:y]}\\in "+e[:open]+e[:binf]+","+e[:bsup]+e[:close]
      end
    end
    instAMP << "}"
    instAMP    
  end
end

def CqlsProba.parseProba(e,opt={:r=>".yyAEP",:aep=>"y",:amp=>"Y",:m=>"m",:mode=>:all})
  parsed_e=CqlsProba.parseFormula(e)
  res={:type=>parsed_e[:type]}
  mode=opt[:mode]
  mode=[:r,:aep,:amp] if !mode or mode==:all
  res[:r]=CqlsProba.aepR(parsed_e,:y=>opt[:r]) if mode.include? :r
  res[:aep]=CqlsProba.aepTex(parsed_e,:y=>opt[:aep],:m=>opt[:m])
  res[:amp]=CqlsProba.ampTex(parsed_e,:y=>opt[:amp])
  res
end

#parseLoiProba(e) avec e de la forme p2, p[2 , p2[, p[2,3[, q0.95, q95%, mean, sd, var
def CqlsProba.parseLoiProba(e,y={:r=>".yyAEP",:aep=>"y",:amp=>"Y"},m="m")
  e2=e.strip
  if ["mean","m","moy","moyenne"].include? e2
    return {:type=>:m,:r=>"mean(#{y[:r]})",:aep=>"\\meanEmp[#{m}]{#{y[:aep]}}",:amp=>"\\EEE{#{y[:amp]}}"}
  elsif ["var","v","sigma2","variance"].include? e2
    return {:type=>:v,:r=>"var(#{y[:r]})",:aep=>"\\Big(\\sdEmp[#{m}]{#{y[:aep]}}\\Big)^2",:amp=>"\\VVV{#{y[:amp]}}"}
  elsif ["sd","standard deviation","ecart-type"].include? e2
    return {:type=>:s,:r=>"sd(#{y[:r]})",:aep=>"\\sdEmp[#{m}]{#{y[:aep]}}",:amp=>"\\sigma(#{y[:amp]})"}
  elsif /^q(\-?\d*\.?\d*)(\%?)$/ =~ e2
    ordre= "("+$1+").to_f"+($2=="%" ? "/100" : "" )
    ordrePercent=CqlsProba.pretty_num(eval(ordre+"*100"))
    {:type=>:q,:r=>"quantile(#{y[:r]},"+eval(ordre).to_s+")",:aep=>"\\quantEmp[#{m}]{#{y[:aep]}}{"+ordrePercent+"\\%}",:amp=>"\\quant{#{y[:amp]}}{"+ordrePercent+"\\%}"}
  elsif /^p([\[\]]?)(\-?\d*\.?\d*)(;?)(\-?\d*\.?\d*)([\[\]]?)$/ =~ e2
    instR="mean(";instAEP="\\meanEmp[#{m}]{";instAMP="\\PPP{"
    # égalité
    if $1+$3+$4+$5=="" and $2!=""
      instR << "#{y[:r]}=="+$2
      instAEP << "#{y[:aep]}="+$2
      instAMP << "#{y[:amp]}="+$2
    else
      if "[]".include? $1 and $2!="" and $3+$4+$5=="" 
        instR << $2+($1=="[" ? " <= " : " < ")+"#{y[:r]}"
        instAEP << "#{y[:aep]}"+($1=="[" ? "\\geq" : ">")+$2
        instAMP << "#{y[:amp]}"+($1=="[" ? "\\geq" : ">")+$2
      elsif "[]".include? $5 and $2!="" and $3+$1+$4=="" 
        instR << "#{y[:r]}"+($5=="[" ? " < " : " <= ")+$2
        instAEP << "#{y[:aep]}"+($5=="[" ? "<" : "\\leq")+$2
        instAMP << "#{y[:amp]}"+($5=="[" ? "<" : "\\leq")+$2
      elsif "[]".include? $1 and $2!="" and $3==";" and "[]".include? $5 and $4!=""
        instR << "("+$2+($1=="[" ? " <= " : " < ")+"#{y[:r]}) & (#{y[:r]}"+($5=="[" ? " < " : " <= ")+$4+")"
        instAEP << "#{y[:aep]}\\in "+$1+$2+","+$4+$5
        instAMP << "#{y[:amp]}\\in "+$1+$2+","+$4+$5
      end
    end
    instR << ")";instAEP << "}";instAMP << "}"
    {:type=>:p,:r=>instR,:aep=>instAEP,:amp=>instAMP}
  end
end

end
