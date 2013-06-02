xquery version "3.0";

import module namespace file = "http://expath.org/ns/file";

declare namespace an = "http://www.zorba-xquery.com/annotations";



declare function local:closetag3($node as node(), $target as node()) as node()*
{

 let $nr := 
  for $node at $i in $node/node()
     return 
     if(local-name($node)=local-name($target) and $node/@*=$target/@*) then
       $i
     else ()

return
(if (count($nr)> 0 ) then
    subsequence($node/node(), 1 , number($nr[1])-1)
  else $node/node()
  ,

for $i at $k in $nr
  return   
  element {local-name($node/node()[$i])}{$node/node()[$i]/@*,
     if($k < count($nr)) then
       subsequence($node/node(), $nr[$k]+1 , $nr[$k+1] - $nr[$k] -1)
     else 
       subsequence($node/node(), $nr[$k]+1 , count($node/node())-$nr[$k] )
   }
)
};

(: ------------------------------------------------------------------------ :)

(: -------------------------------------------------------------- :)
 declare function local:verbatim ( $node as node() ) as item()* {

element {node-name($node)}{$node/@*,
for $node in $node/node()
    return 
        typeswitch($node)
            case text() return 
                (
                
                let $node := local:verb2node($node, 1)

                let $node  := parse-xml(concat('<x>',$node,'</x>'))
                   return $node/x/node()
               )
            default return $node 
}
};  


declare function local:verb2node( $node as xs:string? , $iterator as xs:integer ) as xs:string {

 let $list :=  distinct-values(for $tokens in tokenize($node, '\\verb')[position() > 1]
      return substring($tokens,1,1))

 let $node :=
  if($iterator <= count($list)) then
      let $node := replace($node, concat('\\verb[',$list[$iterator],'](.+?)[',$list[$iterator],']'),  concat('<verb delim="',$list[$iterator],'">$1</verb>'))
      let $node := local:verb2node($node,$iterator)
  return $node
   else $node

 let $iterator := $iterator + 1

return $node
}; 


(: --------------------------------------------------------------- :)

declare function local:closepairs($node as node()*, $target as node()) as node()*
{
 let $nr := 
  for $node at $i in $node
     return 
     if(local-name($node)=local-name($target)) then
       $i
     else ()


return
    (if (count($nr)>0) then
        subsequence($node, 1 , number($nr[1]) - 1)
       else $node  
            ,
    for $i at $k in $nr
       return   
          if($k < count($nr) and $k mod 2 eq 1) then
                element {local-name($node[$i])}{
                  subsequence($node, $nr[$k] + 1 , $nr[$k + 1] - $nr[$k] - 1) 
                }
          
          else if($k < count($nr) and $k mod 2 eq 0) then
            subsequence($node, $nr[$k] + 1 , $nr[$k + 1] - $nr[$k] - 1 ) 
          else if($k = count($nr) and $k mod 2 eq 0) then
           subsequence($node, $nr[$k] + 1 , count($node) - $nr[$k]  )

         else ()
)

};

(: -------------------------------------------------------------- :)


 
 
(: ---------------------------------------------------------------- :)

declare function local:dollar( $node as node() ) as item()* {

let $node2 :=
element {node-name($node)}{$node/@*,
for $node in $node/node()
    return 
        typeswitch($node)
            case text() return 
                (

        let $node := fn:replace($node,'\$','<dollar/>')
        let $node := parse-xml(concat('<text>',$node,'</text>'))
        let $node := local:closepairs($node/text/node(), <dollar/>)
                return $node
               )
            default return $node 
}
 
return
element {node-name($node2)}{$node2/@*,
for $node in $node2/node()
    return 
        typeswitch($node)
           case text() return $node
           case element (dollar) return element mathmode {attribute style {"inline"}, attribute type {"dollar"}, $node/node()}
              default return $node
}



};  

declare function local:parenthesismath ( $node as node() ) as item()* {

element {node-name($node)}{$node/@*,
for $node in $node/node()
    return 
        typeswitch($node)
            case text() return 
                (
        let $node := fn:replace($node,'\\[\(]','<mathmode style="inline" type="parenthesis">')
        let $node := fn:replace($node,'\\[\)]','</mathmode>')
        
        let $node := fn:replace($node,'\\[\[]','<mathmode style="display" type="parenthesis">')
        let $node := fn:replace($node,'\\[\]]','</mathmode>')        
        
        let $node := parse-xml(concat('<text>',$node,'</text>'))
        let $node := <node>{$node}</node>

        return $node/text/node()

               )
            default return $node 
}

};  




(: ---------------------------------------------------------------- :)

declare function local:transform-commands($node as node()) as item()* {

element {node-name($node)}{$node/@*,
for $node in $node/node()
    return 
        typeswitch($node)
            case text() return 
                (
     let $node := fn:replace($node,'\\([a-zA-Z0-9_]+)([*])','<command name="$1" aster="aster"/>')
     let $node := fn:replace($node,'\\([a-zA-Z0-9_]+)','<command name="$1"/>')
     let $node := fn:replace($node,'\\([ ])','<command name="$1"/>')
     let $node := fn:replace($node,'\[','<param type="opt">')
     let $node := fn:replace($node,'\]','</param>')
     let $node := fn:replace($node,'\{','<param>')
     let $node := fn:replace($node,'\}','</param>')
     
   let $text :=
     try {  
         parse-xml(concat('<text>',$node,'</text>'))
     } catch * {
        <error><text><parseerror>{$node}</parseerror></text></error>
     }
     
    return $text/text/node() 

               )
            default return $node 
}
};





(: --------------- special chars -------------------------------- :)
declare function local:transform00($node as element()) as item()* {
    
element {node-name($node)}{$node/@*,    
     let $node := fn:replace($node,'\\&amp;','-amp-')
     let $node := fn:replace($node,'(&lt;)', '-lt-')
     let $node := fn:replace($node,'(&gt;)', '-gt-')
     let $node := fn:replace($node,'(&amp;)','-amp2-')
     let $node := fn:replace($node,'\\[\{]','-openpar-')
     let $node := fn:replace($node,'\\[\}]','-closepar-')
     let $node := fn:replace($node,'\\[\$]','-dollar-')

   let $text :=
     try {  
         parse-xml(concat('<text>',$node,'</text>'))
     } catch * {
        <error><text><parseerror>{$node}</parseerror></text></error>
     }
     
    return $text/text/node() 
}
}; 




(: transform comments :)
declare function local:transform0_1($node as node()) as item()* {

let $node1:=
element {node-name($node)}{$node/@*,
for $node in $node/node()
    return 
        typeswitch($node)
            case text() return 
                (
                   let $node := fn:replace($node,'([^\\][%]+(.*))$','<comment>$1</comment>','m')
                   let $node := parse-xml(concat('<text>',$node,'</text>'))
                   let $node := $node/text/node()
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
            
            default return  $node
}
};
(: ---------------------------------------------------------- :)


(: --------------------------------------------------------------- :)

declare function local:transform0_1x($node as node()) as item()* {

element {node-name($node)}{$node/@*,

for $node in $node/node()
    return 
        typeswitch($node)
            case text() return 
            (
     let $node := fn:replace($node,'\\begin\{([\w]+)[*]\}','<environment name="$1" aster="aster">')
 
     let $node := fn:replace($node,'\\begin\{([\w]+)\}\[(\w+)\]\{([\w]+)\}','<environment name="$1"><param type="opt">$2</param><param>$3</param>')
     let $node := fn:replace($node,'\\begin\{([\w]+)\}\{(\w+)\}\{([\w]+)\}','<environment name="$1"><param>$2</param><param>$3</param>')
     let $node := fn:replace($node,'\\begin\{([\w]+)\}\{(\w+)\}','<environment name="$1"><param>$2</param>')
     let $node := fn:replace($node,'\\begin\{([\w]+)\}\[(\w+)\]','<environment name="$1"><param type="opt">$2</param>')
     let $node := fn:replace($node,'\\begin\{([\w]+)\}','<environment name="$1">')     

     let $node := fn:replace($node,'\\begin\{([\w]+)[*]\}\[(\w+)\]','<environment name="$1" aster="aster"><param type="opt">$2</param>')
     let $node := fn:replace($node,'\\begin\{([\w]+)[*]\}','<environment name="$1" aster="aster">')

     let $node := fn:replace($node,'\\end\{([a-zA-Z0-9]+)\}','</environment>')
     let $node := fn:replace($node,'\\end\{([a-zA-Z0-9]+)[*]\}','</environment>')    
               let $text :=
                    try {  
                      parse-xml(concat('<text>',$node,'</text>'))
                    } catch * {
                      <error><text><parseerror>{$node}</parseerror></text></error>
                    }
             return $text/text/node()
            )
           default return $node 

}   
};
(: --------------------------------------------------------------- :)


declare function local:transform0_5($node0 as node()) as item()* {

if($node0 instance of element() and string($node0/data(@name))!='tabular' and string($node0/data(@name))!='verbatim' and string($node0/data(@name))!='equation') then
(
 let $node := local:fold($node0, 'environment','e')
 let $node := local:verbatim($node)
 let $node := local:fold($node, 'verb','v')
 let $node := local:dollar($node)
 let $node := local:fold($node, 'mathmode','d')
 let $node := local:parenthesismath($node)
 let $node := local:fold($node, 'mathmode','m')
 let $node := local:transform-commands($node)

 let $node := local:unfold($node)

 
 
 let $node :=
  element {node-name($node)}{$node/@*,
    for $node in $node/node()
        return 
        typeswitch($node)
            case text() return $node
            case element(environment) return local:transform0_5($node)
            default return $node
    }
 return $node
)

else $node0

};


(: --------------------------------------------------------------- :)








declare  function local:fold($node as node(), $elementname as xs:string, $sign as xs:string?) as item() {
 
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

 string-join(
    for $node at $x in $node/node()
        return 
            if($node instance of element() and string(node-name($node))=$elementname) then
                string(concat('__', $sign ,'' , $x , '__'))
            else if($node instance of element() and string(node-name($node))='store') then ""
            else $node
        ,'')
}     
};




declare  function local:unfold($node0 as node()) as item()* {

element {node-name($node0)}{$node0/@*,

    let $node2 :=
    for $node in $node0/node()
    return 
         typeswitch($node)
            case text() return 
                (let $node := replace($node, '__([evdm]+[0-9]+)__', '<hideelement nr="$1"/>')
                   let $node := parse-xml(concat('<element>',$node,'</element>'))
                   return $node/element/node()
                )
 
            default return element  {node-name($node)} 
                 {$node/@*, $node/node()}   
                 
    
    for $node in $node2
      return 
          typeswitch($node)
            case text() return $node
            case element(hideelement) return $node0//el[data(@nr)=$node/data(@nr)]/node()
            default return element  {node-name($node)} 
                {$node/@*, $node/node()}   
    
  }
};


declare function local:unfold-comment($node0 as node(), $root as node()) as item()* {

element {node-name($node0)}{$node0/@*,

    let $node2 :=
    for $node in $node0/node()
    return 
         typeswitch($node)
            case text() return 
                (let $node := replace($node, '__([c][0-9]+)__', '<hideelement nr="$1"/>')
                   let $node := parse-xml(concat('<element>',$node,'</element>'))
                   return $node/element/node()
                )
 
            default return element  {node-name($node)} 
                 {$node/@*, $node/node()}   
                 
    
    for $node in $node2
      return 
          typeswitch($node)
            case text() return $node
            case element(hideelement) return $root//el[data(@nr)=$node/data(@nr)]/node()
            case element(environment) return local:unfold-comment($node, $root)
            case element(store) return ()
            default return element  {node-name($node)} 
                {$node/@*, $node/node()}          
  }
};


(: ---------------------------------------------------------------- :)
 declare function local:returnchars($node0 as node()*) as item()* {

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
                return $node
                )
                
             default return local:returnchars($node)
}
};   

(: ---------------------------------------------------------------- :)



(: ------------------------------------------------------------------------ :)

declare function local:index-of2($node as node()) as item()* {
 for $node at $i in $node/node()
 return
    if($node[local-name(.)!='param']) then
               $i
            else ()  
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
  
      
     let $width :=  if(count($sub1/node()) > 0 and 
                       count($sub1/node()[local-name(.)!='param']) = 0 and 
                          count($sub1/node()[local-name(.)='param']) > 0 ) then
                    
                            count($sub1/node()[local-name(.)='param'])
                
                else if(count($sub1/node()) > 0 and 
                        count($sub1/node()[local-name(.)!='param']) > 0 ) then

                            local:index-of2($sub1)[1] - 1
                  else          
                       0
 
      
          return <pair><b>{$i}</b><e>{$i+ $width}</e></pair>

   else 
      let $sub := subsequence($node/node(), $i, count($node/node())-$nr[$k] +1 )
      let $sub1 := <sub>{$sub[position() > 1]}</sub>
     let $width :=  if(count($sub1/node()) > 0 and 
                       count($sub1/node()[local-name(.)!='param']) = 0 
                       and count($sub1/node()[local-name(.)='param']) > 0 ) then
                    
                            count($sub1/node()[local-name(.)='param'])
                
                else if(count($sub1/node()) > 0 and 
                        count($sub1/node()[local-name(.)!='param']) > 0 ) then

                            local:index-of2($sub1)[1] - 1
                  else          
                       0
          return <pair><b>{$i}</b><e>{$i+ $width}</e></pair>
  
    return
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
};

(: ---------------------------------------------------------------- :)
    
declare function local:normalizecommands($node0 as node()) as item()* {

 let $node := local:normalizecommand($node0, <command/>)

 return
 
element {node-name($node)}{$node/@*,

    for $node in $node/node()
    return 
         typeswitch($node)

            case text() return $node
            case element (comment) return comment {$node/node()}
            case element (mathmode) return $node
            case element (verb) return $node
            default return  local:normalizecommands($node)
}

}; 

(: ---------------------------------------------------------------- :)
(: ---------------------------------------------------------------- :)
    
declare function local:sectionsreturns($node as node()*) as item()* {
element  {node-name($node)}{$node/@*,
for $node in $node/node()
    return 
       typeswitch($node)
           case text() return $node
           case comment() return $node
           case element (command) return 
               (
                  if(string(node-name($node)) = 'command' and 
                      ($node/data(@name) = 'section' or $node/data(@name) = 'subsection' or $node/data(@name) = 'item' or $node/data(@name) = 'bibitem')) then
                   (element command {$node/@*},
                        $node/node()
                    )
  else  local:sectionsreturns($node)
               )
               
           default return 
                 local:sectionsreturns($node) 
}
};

(: ---------------------------------------------------------------- :)

declare function local:normalizespecial($node0 as node()*) as item()* {
element  {node-name($node0)}{$node0/@*,
for $node in $node0/node()
    return 
       typeswitch($node)
           case text() return $node
           case comment() return $node
           case element (environment) return  local:normalizespecial(local:closesections($node))
           case element (command) return  local:normalizespecial(local:closesections($node))
           default return 
                 local:normalizespecial($node) 
}
};

declare function local:closesections($node as node()) as node()* { 
element {node-name($node)}{$node/@*,  

if(string(node-name($node)) = 'environment' and $node/data(@name) = 'document') then
    local:closetag3($node, <command name="section"/>)    
 else if(string(node-name($node)) = 'environment' and $node/data(@name) = 'itemize') then
    local:closetag3($node, <command name="item"/>)        
 else if (string(node-name($node)) = 'command' and $node/data(@name) = 'section') then
    local:closetag3($node, <command name="subsection"/>)
 else if(string(node-name($node)) = 'environment' and $node/data(@name) = 'thebibliography') then
    local:closetag3($node, <command name="bibitem"/>)  
 else $node/node()
}
};


(: ---------------------------------------------------------------- :)

declare variable $latexpath as xs:string+ external;



declare %an:nondeterministic function local:latex2xml($latexpath as xs:string) as node()* { 

let $latex := file:read-text($latexpath)

let $latex := 
<latex>{concat('&#10;',$latex)}</latex>

let $doc00 := local:transform00($latex)
let $doc1 := local:transform0_1($doc00)
let $doc2 := local:fold($doc1, 'comment', 'c') 
let $doc3 := local:transform0_1x($doc2)
let $doc4 := local:transform0_5($doc3)
let $doc5 := local:unfold-comment($doc4, $doc4)
let $doc6 := local:returnchars($doc5)
let $latexxml2_1 := local:normalizecommands($doc6)
let $latexxml2_2 := local:sectionsreturns($latexxml2_1)
let $latexxml2_3 :=  local:normalizespecial($latexxml2_2)

return $latexxml2_3
};

let $xml := local:latex2xml($latexpath)


return $xml




