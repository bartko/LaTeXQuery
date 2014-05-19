(:
Copyright (C) 2014 Bartosz Marciniak
marciniak.b@gmail.com
:)

xquery version "3.0";


declare function local:parse-xml($string) as node()*{
  (: util:parse($string) :)
  fn:parse-xml($string)
};




(: --------------- special chars -------------------------------- :)
declare function local:transformspecchar($node as element()) as item()* {
    
element {node-name($node)}{$node/@*,
     let $node := fn:replace($node,'\\&amp;','-amp-')
     let $node := fn:replace($node,'(&lt;)', '-lt-')
     let $node := fn:replace($node,'(&gt;)', '-gt-')
     let $node := fn:replace($node,'(&amp;)','-amp2-')
     let $node := fn:replace($node,'\\[\{]','-openpar-')
     let $node := fn:replace($node,'\\[\}]','-closepar-')
     let $node := fn:replace($node,'\\[\$]','-dollar-')
     let $node := fn:replace($node,'\\[%]','-percent-')
    return $node
}
};


declare function local:returnspecchar($node0 as node()*) as item()* {

element {node-name($node0)}{$node0/@*,

    for $node in $node0/node()
    return
         typeswitch($node)
            case text() return

                (
                let $node := fn:replace($node,'-amp-','\\&amp;')
                let $node := fn:replace($node,'-lt-', '&lt;' )
                let $node := fn:replace($node,'-gt-', '&gt;' )
                let $node := fn:replace($node,'-amp2-', '&amp;')
                let $node := fn:replace($node,'-openpar-','\\{')
                let $node := fn:replace($node,'-closepar-','\\}')
                let $node := fn:replace($node,'-dollar-','\\\$')
                let $node := fn:replace($node,'_polpauza_','&#8211;&#8211;')
                let $node := fn:replace($node,'-percent-','\\%')
                return $node
                )
                
             default return local:returnspecchar($node)
}
};

(: ------------------------------------------------------------------------ :)


(: -------------------------------------------------------------- :)
declare function local:verbatim ( $node as node() ) as item()* {

element {node-name($node)}{$node/@*,
for $node in $node/node()
    return
        typeswitch($node)
            case text() return
                  local:change-first-verb($node)  
            default return $node
}
};



declare function local:change-first-verb ($node as xs:string?) as item()* {

   if(matches($node,'\\verb')) then
     let $firstenv := local:index-of-match-first($node,'\\verb')
     let $tail01   := substring($node, $firstenv)
     let $envname  := substring($tail01, 6, 1)
     let $closefirstenv := local:index-of-match-first(substring($tail01,7), concat('[',$envname,']'))

     let $head := substring($node, 0, $firstenv)
     let $body := substring($tail01, 7, $closefirstenv - 1)
     let $tail := substring($tail01, $closefirstenv + 7 )

     let $bodynode :=  <verb delim="{$envname}">{$body}</verb>

    return 
       ($head , $bodynode , local:change-first-verb($tail))

  else  $node
};

(: --------------------------------------------------------------- :)
(: -- converting dollars to <mathmode> --------------------------- :)


declare function local:dollar( $node as node() ) as item()* {
   element {node-name($node)}{$node/@*,
      for $node in $node/node()
         return
            typeswitch($node)
               case text() return local:transform-first-dollar($node)
               default return $node
    }
};




declare function local:transform-first-dollar($node as xs:string?) as item()* {

if(matches($node,'[$]')) then
let $firstdollar := local:index-of-match-first($node,'[$]')

let $nextcharisdollar := 
    if(substring($node,$firstdollar +1,1)='$') then true()
    else false()

return
  if (not($nextcharisdollar)) then
    let $head := substring($node, 1 , $firstdollar -1)
    let $secdollar := local:index-of-match-first(substring($node,$firstdollar+1),'[$]')
    let $body := substring($node, $firstdollar +1, $secdollar -1)
    let $tail := substring($node, $firstdollar + $secdollar +1 )

    return 
     (
      $head,
         <mathmode type="dollar">{$body}</mathmode>,
         local:transform-first-dollar($tail)
      )
  else
  
    let $head := substring($node, 1 , $firstdollar -1)
    let $secdollar := local:index-of-match-first(substring($node,$firstdollar+2),'[$]')
    let $body := substring($node, $firstdollar +2, $secdollar -1)
    let $tail := substring($node, $firstdollar + $secdollar +3 )
    
    return 
     (
      $head,
         <mathmode type="doubledollar">{$body}</mathmode>,
         local:transform-first-dollar($tail)
      )
else
$node
};


(: ---------------------------------------------------------------- :)

(: transform comments :)
declare function local:transformcomments($node as node()) as item()* {

let $node1:=
element {node-name($node)}{$node/@*,
for $node in $node/node()
    return
        typeswitch($node)
            case text() return
                (
                   let $node := fn:replace($node,'(%.*$)',concat('<comment>','$0','</comment>'),'m')
                   let $node := local:parse-xml(concat('<text>',$node,'</text>'))
                   let $node := $node/text/node()
                   let $node :=
                       for $node2 in $node
                       return
                           if($node2 instance of element() and string(node-name($node2))='comment') then
                               <comment>{substring($node2/node(),2)}</comment>
                           else $node2
                   return $node
                )
          default return $node
}         
    return local:changeincomments($node1)
};


declare function local:changeincomments($node as node()) as item()* {

element {node-name($node)}{$node/@*,
for $node in $node/node()
    return
        typeswitch($node)
            case text() return $node
            case element (comment) return
                   
                    let $node := fn:replace($node,'[-][-]','_polpauza_','m')
                    return <comment>{$node}</comment>
            
            default return $node
}
};

(: ------------------------------------------------------------------------- :)
(: -- converting \[and \( to mathmode -------------------------------------- :)

declare function local:parenthesismath ( $node as node() ) as item()* {

element {node-name($node)}{$node/@*,
for $node in $node/node()
    return
        typeswitch($node)
            case text() return
                (
        let $node := fn:replace($node,'\\[\(]','<mathmode type="parentheses">')
        let $node := fn:replace($node,'\\[\)]','</mathmode>')
        
        let $node := fn:replace($node,'\\[\[]','<mathmode type="brackets">')
        let $node := fn:replace($node,'\\[\]]','</mathmode>')
        
        let $node := local:parse-xml(concat('<text>',$node,'</text>'))
        let $node := <node>{$node}</node>

        return $node/text/node()

               )
            default return $node
}
};

(: ---------------------------------------------------------------- :)



declare function local:transform-commands($node as node()) as item()* {
  let $node:= local:transform-param($node)
  let $node:= local:transform-param-opt($node)  
  return  $node
};


declare function local:transform-param($node as node()) as item()* {

let $node :=
 try{
   element {node-name($node)}{$node/@*,
   for $node in $node/node()
      return
        typeswitch($node)
            case text() return
                (
                 let $node := fn:replace($node,'\\([a-zA-Z0-9_]+)([*])','<command name="$1" aster="aster"/>')
                 let $node := fn:replace($node,'\\([a-zA-Z0-9_]+)','<command name="$1"/>')
                 let $node := fn:replace($node,"\\([ \\,;:!'])",'<command name="$1"/>')


                 let $node := fn:replace($node,'\{','<param>')
                 let $node := fn:replace($node,'\}','</param>')
                 let $text := local:parse-xml(concat('<text>',$node,'</text>'))
                 return $text/text/node()
               )
        default return $node
   }
   } catch *{
     
   element {node-name($node)}{$node/@*, attribute error {'braces'},
         $node/node()
   }
   }

return $node

};




declare function local:transform-param-opt($node as node()) as item()* {

let $node :=
 try{
   element {node-name($node)}{$node/@*,
   for $node in $node/node()
      return
        typeswitch($node)
            case text() return
                (
                let $node := fn:replace($node,'\[','<param type="opt">')
                let $node := fn:replace($node,'\]','</param>')
     
                 let $text := local:parse-xml(concat('<text>',$node,'</text>'))
                 return $text/text/node()
               )
             case element(param) return local:transform-param-opt($node)  
        default return $node
   }
   } catch *{
     
      element {node-name($node)}{$node/@name, 
      
           if($node/@error) then
               attribute error {'braces+squarebrackets'}
           else attribute error {'squarebrackets'}
           ,

             $node/node()
           }
    }
      
   
return $node

};









(: ------------------------------------------------------------------------- :)

declare function local:transform_all($node0 as node()) as item()* {

let $parsedenvironments := local:parsedenvironments() 

let $node:= 

if($node0 instance of element() and 
     (string($node0/data(@name)) = $parsedenvironments 
        or string(node-name($node0))='latex')
    ) then
(
 let $node := local:fold2($node0, 'environment','e')
 let $node := local:dollar($node)
 let $node := local:fold2($node, 'mathmode','d')
 let $node := local:parenthesismath($node)
 let $node := local:fold2($node, 'mathmode','m')
 let $node := local:transform-commands($node)
 let $node := local:unfold($node,$node,'d')
 let $node := local:unfold($node,$node,'m')
 let $node := local:unfold($node,$node,'e')

 let $node :=
  element {node-name($node)}{$node/@*,
    for $node in $node/node()
        return
        typeswitch($node)
            case text() return $node
            case element(environment) return local:transform_all($node)
            default return $node
    }
 return $node
)

else $node0

return $node

};

(: --------------------------------------------------------------- :)




declare function local:fold2($node as node(), $elementname as xs:string, $sign as xs:string?) as item() {
 
element {node-name($node)}{$node/@*,
        $node/store,
            <store elements="{$elementname}">{
                for $node at $x in $node/node()
                return
                    if($node instance of element() and string(node-name($node))=$elementname) then
                        <el nr="{concat($sign,string($x))}">{$node}</el>
                    else ()
            }</store>
 ,

    for $node at $x in $node/node()
        return
            if($node instance of element() and string(node-name($node))=$elementname) then
                string(concat('__', $sign ,'' , $x , '__'))
            else if($node instance of element() and string(node-name($node))='store') then ()
            else $node
       


}
};



declare function local:fold3($node as node(), $elementname as xs:string, $sign as xs:string?) as item() {
 
element {node-name($node)}{$node/@*,
        $node/store,
            <store elements="{$elementname}">{
                for $node at $x in $node/node()
                return
                    if($node instance of element() and string(node-name($node))=$elementname)
                       then
                          if((string($node/data(@name))=local:environmentsnotinpar() and string(node-name($node))='environment') 
                              or 
                          (string($node/data(@name))=local:commandsnotinpar() and string(node-name($node))='command'))
                           then
                             ()
                           else  <el nr="{concat($sign,string($x))}">{$node}</el>
                   
                   else ()
                    
                    
            }</store>
,

let $node0:=
for $node at $x in $node/node()
        return
            if($node instance of element() and string(node-name($node))=$elementname) 
               then
                    if((string($node/data(@name))=local:environmentsnotinpar() and string(node-name($node))='environment') 
                     or 
                    (string($node/data(@name))=local:commandsnotinpar() and string(node-name($node))='command'))
                   then
                      $node    
                  else 
                       string(concat('__', $sign , $x , '__'))



            else if($node instance of element() and string(node-name($node))='store') then ()
            else $node
 
return local:concatstrings($node0)

     
}
};





declare function local:unfold($node0 as node(),$root as node(),$type as xs:string) as item()* {

element {node-name($node0)}{$node0/@*,

    let $node2 :=
    for $node in $node0/node()
    return
         typeswitch($node)
            case text() return
                (let $node := replace($node, concat('__(',$type,'+[0-9]+)__'), '<hideelement nr="$1"/>')
                 let $node := local:parse-xml(concat('<element>',$node,'</element>'))
                   return $node/element/node()
                )
            default return element {node-name($node)}
                 {$node/@*, $node/node()}
         
                 
    for $node in $node2
      return
          typeswitch($node)
            case text() return $node
            case element(hideelement) return $root//el[data(@nr)=$node/data(@nr)]/node()
            case element(environment) return 
         
(:
                 if 
                     ($node/data(@name)=local:parsedenvironments()) 
                 then
                    local:unfold($node, $root,$type)
                      
                 else   
                    $node
:)
           
            local:unfold($node, $root,$type)
            
         
            case element(p) return local:unfold($node, $root,$type)
            case element(param) return local:unfold($node,$root,$type)
            case element(comment) return local:unfold($node,$root,$type) (:do rozwijania verbatim w komentarzach :)
             default return element {node-name($node)}
                {$node/@*, $node/node()}
  }
};


declare function local:removestore($node0 as node()) as item()* {
 element {node-name($node0)}{$node0/@*,
    for $node in $node0/node()
      return
          typeswitch($node)
            case text() return $node
            case element(store) return ()
            case element(environment) return local:removestore($node)
            default return element {node-name($node)}
                {$node/@*, $node/node()}
  }
};


(: ---------------------------------------------------------------- :)


(: ---------------------------------------------------------------- :)
    
declare function local:normalizecommands($node0 as node()) as item()* {

let $node := local:normalizecommand($node0, <command/>)
return
element {node-name($node)}{$node/@*,
    for $node in $node/node()
    return
         typeswitch($node)
            case text() return $node
            case element (mathmode) return $node
            case element (verb) return $node
            default return local:normalizecommands($node)
}
};



declare function local:normalizecommand($node as node(), $target as node()) as node()*
{
 let $nr :=
  for $node at $i in $node/node()
     return
     if(node-name($node)=node-name($target)) then
       $i
     else ()

let $pairs :=
for $i at $k in $nr
  return
  if($k < count($nr)) then
      let $sub := subsequence($node/node(), $i, $nr[$k+1] - $nr[$k])
      let $sub1 := <sub>{$sub[position() > 1]}</sub>
      
      let $width := local:widthofcommand($sub1)

         return <pair><b>{$i}</b><e>{$i+ $width}</e></pair>

   else
      let $sub := subsequence($node/node(), $i, count($node/node())-$nr[$k] + 1)
      let $sub1 := <sub>{$sub[position() > 1]}</sub>
      let $width := local:widthofcommand($sub1)
            
          return <pair><b>{$i}</b><e>{$i+ $width}</e></pair>
  
  let $normalizedcommands :=
element {node-name($node)}{$node/@*,
    (if (count($nr)>0) then
        subsequence($node/node(), 1 , number($nr[1]) - 1)
       else $node/node()
            ,

    for $i at $k in $nr
       return
          if($k < count($nr)) then
          (
            element {local-name($node/node()[$i])}{$node/node()[$i]/@*,
                   subsequence($node/node(), $nr[$k] + 1 , $pairs[b=$i]/e/text()-$i)
                   },
                  subsequence($node/node(), $pairs[b=$i]/e/text() + 1, $nr[$k+1] - ($pairs[b=$i]/e/text() + 1))
           )
           else
           (
            element {local-name($node/node()[$i])}{$node/node()[$i]/@*,
                   subsequence($node/node(), $nr[$k] + 1 , $pairs[b=$i]/e/text()-$i)
                   },
                   subsequence($node/node(), $pairs[b=$i]/e/text() + 1, count($node/node()) - ($pairs[b=$i]/e/text()))
           )
) 
} 

return $normalizedcommands

};



declare function local:widthofcommand($node as node()) as xs:integer {
 
 let $stops:=
  for $node at $i in $node/node()
  return
     if($node[not(local-name(.) = ('param','comment','empty'))]) then
           $i
     else ()
 
 let $width_01 :=
   if($stops[1] <= count($node/node()))
        then $stops[1] -1
   else count($node/node())
        
 let $substring1 := subsequence($node/node(),1,$width_01)
 let $substring2 := local:delcat($substring1)
 
 return count($substring2)
        
};

declare function local:delcat($nodes as node()*) as item()* {
    if(local-name($nodes[last()])=('comment','empty')) then
       local:delcat(subsequence($nodes, 1, count($nodes)-1))
    else $nodes
};




(: ---------------------------------------------------------------- :)

declare function local:commentsreturn($node0 as node()) as item()* {

let $parsedenvironments := local:parsedenvironments() 

let $node:=

if($node0 instance of element() and 
     (string($node0/data(@name)) = $parsedenvironments  
       or string(node-name($node0))='latex' or string(node-name($node0))=('p','param','command'))
     ) then

   ( 
    element {node-name($node0)}{$node0/@*,
    for $node in $node0/node()
    return
         typeswitch($node)
            case text() return $node
            case element (comment) return comment {$node/node()}
            default return local:commentsreturn($node)
    }
   )


   else 
   (
     element {node-name($node0)}{$node0/@*,
     for $node in $node0/node()
      return
         typeswitch($node)
            case text() return $node
            case element (comment) return concat('%',$node/node())
            default return local:commentsreturn($node)
    }
   )

return $node

};







(: ----------------------------------------------------------------- :)

(: resotore commands verb nested in anather verb, verbatim or comments :)

declare function local:verbreturn($node as node()) as item()* {
element {node-name($node)}{$node/@*,
    for $node in $node/node()
    return
         typeswitch($node)
            case text() return $node
            case element (comment) return local:verb2tex($node)
            case element (environment) return 
              if($node/data(@name)='verbatim') then
                  local:verb2tex($node)            
              else if(not($node/data(@name)=local:parsedenvironments())) then 
                  local:verb2tex($node)

              else local:verbreturn($node)
            
            default return local:verbreturn($node)
}
};


declare function local:verb2tex($node as node()) as item()* {
element {node-name($node)}{$node/@*,
    for $node in $node/node()
    return
         typeswitch($node)
            case text() return $node
            case element (verb) return 
                    
                    concat('\verb',$node/data(@delim),$node/text(),$node/data(@delim))
                    
            default return $node
}
};



(: ---------------------------------------------------------------- :)
    
declare function local:sectionsreturns($node as node()*) as item()* {
element {node-name($node)}{$node/@*,
for $node in $node/node()
    return
       typeswitch($node)
           case text() return $node
           case comment() return $node
           case element (command) return
               (
                if(string(node-name($node)) = 'command' and
                          $node/data(@name)=(local:sectionsorder(),('item','bibitem'))
                  ) then
                    (element command {$node/@*},
                        $node/node()
                    )
                 else local:sectionsreturns($node)
               )
               
           default return
                 local:sectionsreturns($node)
}
};

(: ---------------------------------------------------------------- :)


declare function local:normalizespecial($node0 as node()) as item()* {

(: gdy dokument nie ma begin{document}  :)

if(local-name($node0)='latex' and not(exists($node0/node()[data(@name) = ('document')]))) then
  let $latex :=
   <latex>{
   local:closesections2($node0/node(), local:sectionsorder())
    }</latex>
 
   let $latex:=
     element {node-name($latex)}{$latex/@*,
     for $node in $latex/node()
     return
         typeswitch($node)
            case text() return $node
            case comment() return $node
            default return
                 local:normalizespecial($node)           
     }

  return $latex

else                       

element {node-name($node0)}{$node0/@*,
for $node in $node0/node()
    return
    
       typeswitch($node)
           case text() return $node
           case comment() return $node
           case element (environment) 
                        return 
                        
                         if($node/data(@name) = 'document') then
                            local:normalizespecial(element {node-name($node)}{$node/@*,
                                 local:closesections2($node/node(), local:sectionsorder())
                         })
                            
                         
                         else if($node/data(@name) = ('itemize','enumerate')) then
                           local:normalizespecial(element {node-name($node)}{$node/@*,
                                 local:closesections2($node/node(), 'item')
                                 
                         })
                         
                         
                         else if($node/data(@name) = ('thebibliography')) then
                           local:normalizespecial(element {node-name($node)}{$node/@*,
                                 local:closesections2($node/node(), 'bibitem')
                                 
                         })
                         
                         
                        else
                           $node
           

                 default return
                 local:normalizespecial($node)
}
};

(: --------normalize sections------------------------------------------------------------------- :)
(: --------------------------------------------------------------------------------------------- :)
declare function local:index-of-first($node as item()*, $target as xs:string*) as item()* {
 let $indexes:=
 for $node at $i in $node
 return
    if($node instance of element() and local-name($node)="command" and $node/data(@name)=$target) then
               $i
            else 0
  return
  if($indexes != 0)
     then $indexes[.!=0][1]
     else 0
};

declare function local:index-of-first2($node as xs:string*, $target as xs:string*) as item()* {
 let $indexes:=
 for $node at $i in $node
 return
    if($node=$target) then
               $i
            else ()
  return $indexes[1]          
};

declare function local:name-of-first($node as item()*, $target as xs:string*) as item()* {
      let $pos := local:index-of-first($node, $target)
      return
      if ($pos!=0) then
           $node[$pos]/data(@name)
      else ""
};

declare function local:elements-before($items as item()*, $target as xs:string) as item()* {
  if($target!="") then
  subsequence($items, 1 , local:index-of-first2($items, $target))
  else $items
};


declare function local:closesections2($doc as item()*, $target as xs:string*) as item()*{

let $firstsec := local:index-of-first($doc, $target)
let $firstname := local:name-of-first($doc, $target)
let $parenttarget := local:elements-before($target, $firstname)
let $tail := $doc[position() > $firstsec]
let $nextsec := local:index-of-first($tail, $parenttarget)

let $nextsec := if($nextsec!= 0) then
                   $nextsec + $firstsec
                else count($tail) + $firstsec + 1

let $norm:=
    if($firstsec = 0) then
      $doc
    else 
      (
        $doc[position()< $firstsec],
          let $element :=
          element {local-name($doc[$firstsec])}{$doc[$firstsec]/@*,
              local:closesections2($doc[position() > $firstsec and position() < $nextsec],$target)
              }
          return  local:normalize-right($element)   
         ,
         local:closesections2($doc[position()>= $nextsec],$target)
       )
return $norm
};
          
(: -------- end of normalize sections ---------------------------------------------------------- :)
(: --------------------------------------------------------------------------------------------- :) 



(: normalize element 
'<A><a>text</a> text  </A>'
'<A><a>text</a> text</A>  '
:)
declare function local:normalize-right($node as node()) as item()*
{
 
    if($node/node()[last()] instance of text())
       then
       (
        element {local-name($node)}{$node/@*,
          (
          $node/node()[position()< count($node/node())]
          ,
             replace($node/node()[count($node/node())],'\s+$',''))
        }
      ,
         let $textwidthtrim := string-length(replace($node/node()[last()],'\s+$',''))
         return substring($node/node()[last()],$textwidthtrim +1)
      )
      
       else
        element {local-name($node)}{$node/@*,
          $node/node()
        }
   
};
(: ---------------------------------------------------------------- :)



(: ---------------------------------------------------------------- :)

declare function local:emptytemp($node as node()) as item() {
element {node-name($node)}{$node/@*,
    for $node in $node/node()
    return
         typeswitch($node)
            case text() return 
                 if (normalize-space($node) = '')
                    then <empty value="{$node}"/>
                 else
                   $node
            
            default return local:emptytemp($node)
}
};



declare function local:emptytempremove($node as node()) as item() {
element {node-name($node)}{$node/@*,
    for $node in $node/node()
    return
    typeswitch($node)
            case text() return $node
            case element (empty) return $node/data(@value)
            default return local:emptytempremove($node)
}
};




(: ----------------------------------------------------- :)

declare function local:index-of-match-first 
  ( $arg as xs:string? ,
    $pattern as xs:string )  as xs:integer? {
       
  if (matches($arg,$pattern))
  then string-length(tokenize($arg, $pattern)[1]) + 1
  else ()
 } ;
 
 




 (: ----transform environmentst -------------------------------------------- :)
 (: ------------------------------------------------------------------------ :)
 
 declare function local:index-of-next-env( $string as xs:string?)  as item()* {
     
  if (matches($string,'\\begin|\\end')) then

    let $firstenv := local:index-of-match-first($string,'\\begin|\\end')
    let $tail01   := substring($string, $firstenv)
    let $envtype  := substring($tail01, 2,3)
    let $beginpar := local:index-of-match-first($tail01,'\{')
    let $envwidth := local:index-of-match-first($tail01,'\}')
    let $envname  := substring($tail01, $beginpar + 1, $envwidth - $beginpar - 1)
    let $envend   := $firstenv + $envwidth -1
   
    return ($firstenv,$envname, $envend, $envtype,$envwidth)
    
  else ()
 } ;
 

 declare function local:index-of-next-env2( $string as xs:string?, $envname as xs:string? )  as item()* {
      
   let $envname :=
    if (matches($envname,'^.+[*]$')) then replace($envname, '^(.+)[*]$', '$1[*]')
    else $envname
  
  
  return
    
      
      
  if (matches($string,concat('(\\begin|\\end)\s*\{',$envname,'\}'))) then

    let $firstenv := local:index-of-match-first($string,concat('(\\begin|\\end)\s*\{',$envname,'\}'))
    let $tail01   := substring($string, $firstenv)
    let $envtype  := substring($tail01, 2,3)
    let $beginpar := local:index-of-match-first($tail01,'\{')
    let $envwidth := local:index-of-match-first($tail01,'\}')
    let $envend   := $firstenv + $envwidth -1
   
    return ($firstenv,'',$envend, $envtype, $envwidth)
    
  else ()
 } ;


declare function local:closeenv($doc as text()) as node()* {
        
  if(matches($doc,'\\begin\{|\\end\{')) then  
  
   let $firstenv := local:index-of-next-env($doc)   
   let $closeenv := local:findclose($doc,$firstenv[3] + 1, $firstenv[2], 1)  


    let $firstenv_2 :=
    if (matches($firstenv[2],'^.+[*]$')) then replace($firstenv[2], '^(.+)[*]$', '$1')
    else $firstenv[2]

    let $aster :=
    if (matches($firstenv[2],'^.+[*]$')) then true()
    else false()


     
   return (text{substring($doc,1,$firstenv[1] - 1)},
           element{"environment"}{attribute name { $firstenv_2 }, 
                   
                   if($aster) then attribute aster { "aster" }
                   else ""
                   ,
                    substring($doc, $firstenv[3] + 1, $closeenv[1] - $firstenv[3] - 1) 
                   },       
          local:closeenv(text{substring($doc, $closeenv[3] + 1)})
          )        
  else
  $doc 
} ;



declare function local:findclose($doc as xs:string, $startat as xs:integer, $envname as xs:string, $level as xs:integer) as item()* {

  let $doctemp := substring ($doc, $startat) 
  let $nextenv := local:index-of-next-env2($doctemp, $envname)
  let $level   := if($nextenv[4]='beg') then $level +1 else $level -1
  let $newstartat   := $startat + $nextenv[3]   
  let $nextenvbegin := $startat + $nextenv[1] -1
     
  return  
   if ($level > 0) then 
      local:findclose($doc,$newstartat, $envname,$level)
   else ($nextenv[1] + $startat -1,'',$nextenv[3] + $startat - 1, $nextenv[4], $nextenv[5])
} ;






(: ------------------------------------------------------------------------ :)

(: to chyba mozna uproscic :)
declare function local:transformenvironm($node as node()*) as item()* {
element {node-name($node)}{$node/@*,
    for $node in $node/node()
        return
        typeswitch($node)
            case text() return 
                 let $nodswithenv := local:closeenv($node)
                 return 
                     
                     for $node in $nodswithenv
                           return 
                             typeswitch($node)
                                case text() return local:closeenv($node)

                                case element() return
                                    if(local-name($node)='environment' and 
                                         $node/data(@name)=local:parsedenvironments())
                                              then local:transformenvironm($node)
                                    else $node

                                default return $node
                           
            default return $node
}    
};




declare function local:splittexttoparagraphs($node as xs:string?) as item()* {

if(normalize-space($node)!='') then 
  
  let $beginp := local:index-of-match-first($node,'[a-zA-Z0-9_]') 

  let $withoutwhiteatbegin:= substring($node,$beginp)    

  let $endp := 
    if(local:index-of-match-first($withoutwhiteatbegin,'\s*\n\s*\n\s*([a-zA-Z0-9_]+)\s*') !=0 ) then
         local:index-of-match-first($withoutwhiteatbegin,'\s*\n\s*\n\s*([a-zA-Z0-9_]+)\s*')
    else if (local:index-of-match-first($withoutwhiteatbegin,'(\s+)$')!=0 ) then
         local:index-of-match-first($withoutwhiteatbegin,'(\s+)$')
    else string-length($withoutwhiteatbegin) +1
   
   
  let $firswhitechars := substring($node,1,$beginp -1)
  let $firspar := substring($withoutwhiteatbegin, 1 ,$endp -1)
  let $restpars:= substring($withoutwhiteatbegin, $endp)

  return ($firswhitechars, <p>{$firspar}</p>, local:splittexttoparagraphs($restpars))

else $node   
};           





declare function local:textintopar($node0 as node()) as item()* {

(: intersection - <p> onli in parsed environments :)
let $enironmentswithpar := distinct-values(local:parsedenvironments()[.=local:environmentswithpar()])

let $commandswitypar :=  local:commandswithpar()


let $node := $node0

let $node :=
  element {node-name($node)}{$node/@*,
    for $node in $node/node()
        return
        typeswitch($node)
            case text() return $node
            case element() return 
                     
                   if( 
                     (string(node-name($node))='environment' and $node/data(@name)=$enironmentswithpar)
                      or    
                     (string(node-name($node))='command' and $node/data(@name)=$commandswitypar  )
                     )
                     then
                      
                          let $node := local:fold3($node, 'command','comd')
                          let $node := local:fold3($node, 'environment','env')
                          let $node := local:fold3($node, 'verb','verb')
                          let $node := local:fold3($node, 'comment','comt')
                          let $node := local:fold3($node, 'mathmode','math')
                          let $node := local:fold3($node, 'param','param')
                          
                          let $node :=
                             element {node-name($node)}{$node/@*,
                                 for $node in $node/node()
                                 return
                                 typeswitch($node)
                                 case text() return 
                                             
                                       local:splittexttoparagraphs($node)
                                     
                                 default return $node
                                 
                         } 
                          let $node := local:unfold($node,$node,'param') 
                          let $node := local:unfold($node,$node,'math')                   
                          let $node := local:unfold($node,$node,'comt') 
                          let $node := local:unfold($node,$node,'verb')
                          let $node := local:unfold($node,$node,'env')
                          let $node := local:unfold($node,$node,'comd') 

                      
                          let $node := local:removestore($node)
                       
                          return local:textintopar($node)
                          
                  else local:textintopar($node)

            default return local:textintopar($node)
    }
 return $node

};





declare function local:removepar($node as node(), $nodename as xs:string) as item()* {

  element {node-name($node)}{$node/@*,
    for $node in $node/node()
        return
        typeswitch($node)
            case text() return $node
            case element(p) 
                return 
                   if(local:ifcontainsonlyelements($node, $nodename) and
                     not(local:ifcontainstext($node))) then
                        $node/node()
                   else 
                        local:removepar($node,$nodename)
            default return local:removepar($node,$nodename)
          }
};

declare function local:ifcontainsonlyelements($node0 as node(), $nodename as xs:string ) as xs:boolean {
       let $elements := 
        distinct-values(
           for $node in $node0/node()
           return
           if($node instance of element()) then
             local-name($node)
           else())
         

       let $contains:=  
        if($nodename = $elements and count($elements)=1) then true()
        else false()   
  
       return $contains

(:

       return
       
       if($contains and count($elements)>1) then false()
       else true()    
:)       
      
      
};

declare function local:ifcontainstext($node0 as node()) as xs:boolean {
       let $textexists :=
       for $node in $node0/text()[normalize-space(.) != '']
                       return $node
       return
       if (count($textexists)>0) then true() else false()                              
};

(:
declare function local:ifcontainsemptytext($node0 as node()) as xs:boolean {
       let $textexists :=
       for $node in $node0/text()[normalize-space(.) = '']
                       return $node
       return
       if (count($textexists)>0) then true() else false()                              
};
:)
(:
declare function local:ifcontainselementandother($node as node(), $nodename as xs:string) as xs:boolean {
       let $aa := 
        distinct-values(
           for $node in $node/node()
           return
           if($node instance of element()) then
             local-name($node)
           else())
         
       return if($nodename = $aa) then true()
       else false()    
           
};
:)



declare function local:correctpar($node0 as node()) as item()* {


let $commandtocorrect :=  distinct-values((local:sectionsorder(),'item'))

let $node := $node0

let $node :=
  element {node-name($node)}{$node/@*,
    for $node in $node/node()
        return
        typeswitch($node)
            case text() return $node
            case element(command) return 
                 if($node/data(@name)=$commandtocorrect and $node/p) then
                 
                 let $element :=
                 element {node-name($node)}{$node/@*,
                         
                         let $fistparagraphpos:=
                           subsequence(
                              for $node at $i in $node/node()
                                return
                              if(local-name($node)='p') then $i
                              else (),
                            1,1)   
                             
                             return 
                             ($node/node()[position()< $fistparagraphpos] ,
                            
                              ( if($node/data(@name)=local:sectionsorder()) then
                                 local:correctparinsections($node/node()[position() = $fistparagraphpos])
                               else if($node/data(@name)=('item')) then 
                                 local:correctparinitemize($node/node()[position() = $fistparagraphpos])
                               else ()
                           ),
                              $node/node()[position()> $fistparagraphpos]
                          )            
                     }
                     return local:correctpar($element)
                     
                     
 

                   else local:correctpar($node)
                   
            default return local:correctpar($node)
    }
 return $node

};


declare function local:correctparinsections($node0 as node()) as item()* {

   let $parampos:=
   for $node at $i in $node0/node()
   return
    if(local-name($node)='param' and not($node/data(@type))) then
               $i
            else ()
           
   let $parampos:=$parampos[1]        
           
          
           
   let $labelpos:=
   for $node at $i in $node0/node()[position()>$parampos]
   return
    if(local-name($node)='command' and $node/data(@name)='label') then
               $i
            else ()
   
   let $labelpos:=
   if ($labelpos<=2) then 
       $labelpos + $parampos
      else ()
   let $endcommand := if($labelpos>0) then $labelpos
    else $parampos  
        
            
   return (
           $node0/node()[position()<=$endcommand],
           local:normalizep(<p>{$node0/node()[position()>$endcommand]}</p>)
          )   

};




declare function local:correctparinitemize($node0 as node()) as item()* {

   let $firstnodeemptytext :=
     if($node0/node()[1] instance of text() and normalize-space($node0/node()[1])='') then true()
     else false()

   let $parampos:=
   subsequence(for $node at $i in $node0/node()
   return
    if(local-name($node)='param' and $node/data(@type)='opt') then
               $i
            else (),1,1)
         
    let $parampos2 :=
     if($parampos = 2 and $firstnodeemptytext) then $parampos
     else if ($parampos = 1) then $parampos
     else 0

   let $tail := <p>{$node0/node()[position()> $parampos2]}</p>
  
   return (
     $node0/node()[position()<= $parampos2],
     local:normalizep($tail)
   )

};

(: uogolnic to na rozne elementy :)
(: <p>  text</p> change to "  <p>text</p>" :)
declare function local:normalizep($tail as node()) as item()* {

     let $tail_a := 
             if($tail/node()[1] instance of text() and normalize-space($tail/node()[1])='') then 
               ($tail/node()[1],<p>{$tail/node()[position()>1]}</p>)


             else if($tail/node()[1] instance of text() and 
                               not(normalize-space($tail/node()[1])='')) then 
              
                 let $whitewidth := local:index-of-match-first($tail/node()[1],'[a-zA-Z0-9_]') -1
                  return 
                  (substring($tail/node()[1],1,$whitewidth),<p>{substring($tail/node()[1],$whitewidth +1),$tail/node()[position()>1]}</p>)
               
             else if($tail/node()[1] instance of element()) then
             <p>{$tail/node()}</p>
             
             else ()

  return $tail_a
  

};

(:
(<d/>,"mm","dd",<m/>,"mm","dd")
zamienia na (<d/>,"mmdd",<m/>,"mmdd")
:)
declare function local:concatstrings($node as item()*) as item()* {
let $indx:=
     for $x at $i in $node
     return
     if(not($node[$i] instance of element()) and not($node[$i+1] instance of element())) then $i
     else ()
     let $indx := if(count($node)=$indx[last()]) then
               subsequence($indx,1,count($indx)-1)
               else $indx

      
     let $ind := if(count($indx)>0) then $indx[1]
                 else 0
     
let $node:=
     
      if($ind = 0)then
         $node
      else 
       ($node[position()<$ind]
       ,
        local:concatstrings((concat( $node[position()=$ind], $node[position()= $ind +1]),
        let $tail := $node[position()> $ind +1]
        return $tail))
      )
      
   return $node
};



(: ----- settings block :)


declare function local:settingsdoc() as item() {
   doc($settings-document)
};



declare function local:sectionsorder() as item()* {
   let $tokens := tokenize(local:settingsdoc()//sectionsorder/node(),',')
   return 
      for $token in $tokens
        return normalize-space($token)
};


declare function local:parsedenvironments() as item()* {
    let $tokens := tokenize(local:settingsdoc()//parsedenvironments/node(),',')
    return 
       for $token in $tokens
       return normalize-space($token)
};


declare function local:environmentswithpar() as item()* {
    let $tokens := tokenize(local:settingsdoc()//environmentswithpar/node(),',')
    return 
        for $token in $tokens
        return normalize-space($token)
};


declare function local:commandswithpar() as item()* {
   let $tokens := tokenize(local:settingsdoc()//commandswithpar/node(),',')
   return 
       for $token in $tokens
       return normalize-space($token)
};


declare function local:environmentsnotinpar() as item()* {
    let $tokens := tokenize(local:settingsdoc()//environmentsnotinpar/node(),',')
    return 
        for $token in $tokens
        return normalize-space($token)
};

declare function local:commandsnotinpar() as item()* {
    let $tokens := tokenize(local:settingsdoc()//commandsnotinpar/node(),',')
    return 
        for $token in $tokens
        return normalize-space($token)
};





(: ---------------------------------------------------------------- :)

declare function local:latex2xml($latexpath as xs:string) as node()* {

let $latex := file:read-binary($latexpath)

let $latex := convert:binary-to-string($latex)

let $latex :=<latex>{$latex}</latex>

let $latex := local:transformspecchar($latex)        (: special chars :)

let $latex := local:verbatim($latex)                 (: fold verb :)

let $latex := local:fold2($latex, 'verb','v')

let $latex := local:transformcomments($latex)        (: transform comments :)

let $latex := local:fold2($latex, 'comment', 'c')    (: fold comments :)

let $latex := local:transformenvironm($latex)        (: transform environments :)

let $latex := local:transform_all($latex)             (: transofrm commands, mathmode, verbatim ... :)

let $latex := local:unfold($latex,$latex,'c')

let $latex := local:unfold($latex,$latex,'v')

let $latex := local:removestore($latex)

let $latex := local:emptytemp($latex)

let $latex := local:normalizecommands($latex)

let $latex := local:emptytempremove($latex)

let $latex := local:verbreturn($latex)      (: replacing <verb> node with \verb!! nested in other verbatims and comments :)
let $latex := local:sectionsreturns($latex)

let $latex := local:normalizespecial($latex)

let $latex := local:textintopar($latex)    (: split texts into paragraphs  <p> :)

let $latex := local:returnspecchar($latex)

let $latex := local:correctpar($latex)     (: <command name="section"> <p><param>First chapter</param> to 
                                             <command name="section"> <param>First chapter</param><p> :)

let $latex := local:removepar($latex,'comment')      (: remove <par> from around comments :)

let $latex := local:commentsreturn($latex)    (: replacing <comment> node fith real comments: <!-- -->  :)

return $latex
};
 

let $params := map { "indent" := "no" }

let $xml := local:latex2xml($input-document)

return file:write($output-document, $xml, $params) 




