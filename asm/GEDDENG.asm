*          DATA SET GEDDENG    AT LEVEL 001 AS OF 01/17/91                      
*PHASE T00DF0                                                                   
         TITLE 'GENERAL - DICTIONARY - ENGLISH'                                 
GEDDENG  CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'GEDDENG '                                                    
         DC    AL4(BASEX-BASE-PANACEA)                                          
         DC    AL2((DDNDXX-DDNDX-2)/2)                                          
         DC    AL2(0)                                                           
         SPACE 2                                                                
DDNDX    DC    AL2(0)                                                           
*                                                                               
         DC    AL2(DD0001-BASE)    active                                       
         DC    AL2(DD0002-BASE)    by id                                        
         DC    AL2(DD0003-BASE)    change no                                    
         DC    AL2(DD0004-BASE)    change reason                                
         DC    AL2(DD0005-BASE)    cursor address                               
         DC    AL2(DD0006-BASE)    disk address                                 
         DC    AL2(DD0007-BASE)    display                                      
         DC    AL2(DD0008-BASE)    help                                         
         DC    AL2(DD0009-BASE)    id                                           
*                                                                               
         DC    AL2(DD0010-BASE)    security level                               
         DC    AL2(DD0011-BASE)    list                                         
         DC    AL2(DD0012-BASE)    me                                           
         DC    AL2(DD0013-BASE)    next                                         
         DC    AL2(DD0014-BASE)    nodata                                       
         DC    AL2(DD0015-BASE)    password                                     
         DC    AL2(DD0016-BASE)    record added                                 
         DC    AL2(DD0017-BASE)    record length                                
         DC    AL2(DD0018-BASE)    rep                                          
         DC    AL2(DD0019-BASE)    req                                          
         DC    AL2(DD0020-BASE)    security                                     
*                                                                               
         DC    AL2(DD0021-BASE)    today                                        
         DC    AL2(DD0022-BASE)    asap                                         
         DC    AL2(DD0023-BASE)    dds                                          
         DC    AL2(DD0024-BASE)    now                                          
         DC    AL2(DD0025-BASE)    on                                           
         DC    AL2(DD0026-BASE)    ov                                           
         DC    AL2(DD0027-BASE)    soon                                         
         DC    AL2(DD0028-BASE)    no                                           
         DC    AL2(DD0029-BASE)    first                                        
         DC    AL2(DD0030-BASE)    last                                         
*                                                                               
         DC    AL2(DD0031-BASE)    this                                         
         DC    AL2(DD0032-BASE)    exit                                         
         DC    AL2(DD0033-BASE)    x                                            
         DC    AL2(DD0034-BASE)    add                                          
         DC    AL2(DD0035-BASE)    change                                       
         DC    AL2(DD0036-BASE)    delete                                       
         DC    AL2(DD0037-BASE)    restore                                      
         DC    AL2(DD0038-BASE)    select                                       
         DC    AL2(DD0039-BASE)    january                                      
         DC    AL2(DD0040-BASE)    february                                     
*                                                                               
         DC    AL2(DD0041-BASE)    march                                        
         DC    AL2(DD0042-BASE)    april                                        
         DC    AL2(DD0043-BASE)    may                                          
         DC    AL2(DD0044-BASE)    june                                         
         DC    AL2(DD0045-BASE)    july                                         
         DC    AL2(DD0046-BASE)    august                                       
         DC    AL2(DD0047-BASE)    september                                    
         DC    AL2(DD0048-BASE)    october                                      
         DC    AL2(DD0049-BASE)    november                                     
         DC    AL2(DD0050-BASE)    december                                     
*                                                                               
         DC    AL2(DD0051-BASE)    monday                                       
         DC    AL2(DD0052-BASE)    tuesday                                      
         DC    AL2(DD0053-BASE)    wednesday                                    
         DC    AL2(DD0054-BASE)    thursday                                     
         DC    AL2(DD0055-BASE)    friday                                       
         DC    AL2(DD0056-BASE)    saturday                                     
         DC    AL2(DD0057-BASE)    sunday                                       
         DC    AL2(DD0058-BASE)    run date                                     
         DC    AL2(DD0059-BASE)    run time                                     
         DC    AL2(DD0060-BASE)    page                                         
*                                                                               
         DC    AL2(DD0061-BASE)    subpage                                      
         DC    AL2(DD0062-BASE)    period                                       
         DC    AL2(DD0063-BASE)    requestor                                    
         DC    AL2(DD0064-BASE)    on                                           
         DC    AL2(DD0065-BASE)    created on                                   
         DC    AL2(DD0066-BASE)    at                                           
         DC    AL2(DD0067-BASE)    run on                                       
         DC    AL2(DD0068-BASE)    o (overnight)                                
         DC    AL2(DD0069-BASE)    error                                        
         DC    AL2(DD0070-BASE)    n/d                                          
*                                                                               
DDNDXX   DC    AL2(65535)                                                       
         SPACE 2                                                                
DDTXT    DS    0C                                                               
*                                                                               
DD0001   DC    AL1(2,3,6),C'act',C'active'                                      
DD0002   DC    AL1(1,5),C'by ID'                                                
DD0003   DC    AL1(1,10),C'change no.'                                          
DD0004   DC    AL1(1,13),C'change reason'                                       
DD0005   DC    AL1(1,14),C'cursor address'                                      
DD0006   DC    AL1(1,12),C'disk address'                                        
DD0007   DC    AL1(3,2,3,7),C'd ',C'dis',C'display'                             
DD0008   DC    AL1(2,4,8),C'help',C'help    '                                   
DD0009   DC    AL1(1,2),C'id'                                                   
*                                                                               
DD0010   DC    AL1(1,14),C'security level'                                      
DD0011   DC    AL1(2,4,8),C'list',C'list    '                                   
DD0012   DC    AL1(1,2),C'me'                                                   
DD0013   DC    AL1(2,4,8),C'next',C'next    '                                   
DD0014   DC    AL1(1,7),C'no data'                                              
DD0015   DC    AL1(2,4,8),C'pass',C'password'                                   
DD0016   DC    AL1(1,12),C'record added'                                        
DD0017   DC    AL1(1,13),C'record length'                                       
DD0018   DC    AL1(3,2,3,6),C'p ',C'rep',C'report'                              
DD0019   DC    AL1(1,3),C'req'                                                  
*                                                                               
DD0020   DC    AL1(1,8),C'security'                                             
DD0021   DC    AL1(1,5),C'today'                                                
DD0022   DC    AL1(1,4),C'asap'                                                 
DD0023   DC    AL1(1,3),C'dds'                                                  
DD0024   DC    AL1(1,3),C'now'                                                  
DD0025   DC    AL1(1,2),C'on'                                                   
DD0026   DC    AL1(1,2),C'ov'                                                   
DD0027   DC    AL1(1,4),C'soon'                                                 
DD0028   DC    AL1(1,2),C'no'                                                   
DD0029   DC    AL1(2,5,8),C'first',C'first   '                                  
*                                                                               
DD0030   DC    AL1(2,4,8),C'last',C'last    '                                   
DD0031   DC    AL1(2,4,8),C'this',C'this    '                                   
DD0032   DC    AL1(2,4,8),C'exit',C'exit    '                                   
DD0033   DC    AL1(2,2,8),C'x ',C'x       '                                     
DD0034   DC    AL1(3,2,3,3),C'a ',C'add',C'add'                                 
DD0035   DC    AL1(3,2,3,6),C'c ',C'cha',C'change'                              
DD0036   DC    AL1(3,2,3,6),C'd ',C'del',C'delete'                              
DD0037   DC    AL1(3,2,3,7),C'r ',C'res',C'restore'                             
DD0038   DC    AL1(4,2,3,6,8),C's ',C'sel',C'select',C'select  '                
DD0039   DC    AL1(2,3,7),C'jan',C'january'                                     
DD0040   DC    AL1(2,3,8),C'feb',C'february'                                    
DD0041   DC    AL1(2,3,5),C'mar',C'march'                                       
DD0042   DC    AL1(2,3,5),C'apr',C'april'                                       
DD0043   DC    AL1(2,3,3),C'may',C'may'                                         
DD0044   DC    AL1(2,3,4),C'jun',C'june'                                        
DD0045   DC    AL1(2,3,4),C'jul',C'july'                                        
DD0046   DC    AL1(2,3,6),C'aug',C'august'                                      
DD0047   DC    AL1(2,3,9),C'sep',C'september'                                   
DD0048   DC    AL1(2,3,7),C'oct',C'october'                                     
DD0049   DC    AL1(2,3,8),C'nov',C'november'                                    
DD0050   DC    AL1(2,3,8),C'dec',C'december'                                    
DD0051   DC    AL1(2,3,6),C'mon',C'monday'                                      
DD0052   DC    AL1(2,3,7),C'tue',C'tuesday'                                     
DD0053   DC    AL1(2,3,9),C'wed',C'wednesday'                                   
DD0054   DC    AL1(2,3,8),C'thu',C'thursday'                                    
DD0055   DC    AL1(2,3,6),C'fri',C'friday'                                      
DD0056   DC    AL1(2,3,8),C'sat',C'saturday'                                    
DD0057   DC    AL1(2,3,6),C'sun',C'sunday'                                      
DD0058   DC    AL1(1,8),C'run date'                                             
DD0059   DC    AL1(1,8),C'run time'                                             
DD0060   DC    AL1(1,4),C'page'                                                 
DD0061   DC    AL1(1,7),C'subpage'                                              
DD0062   DC    AL1(1,6),C'period'                                               
DD0063   DC    AL1(1,9),C'requestor'                                            
DD0064   DC    AL1(1,2),C'on'                                                   
DD0065   DC    AL1(1,10),C'created on'                                          
DD0066   DC    AL1(1,2),C'at'                                                   
DD0067   DC    AL1(1,6),C'run on'                                               
DD0068   DC    AL1(1,2),C'o '                                                   
DD0069   DC    AL1(1,5),C'error'                                                
DD0070   DC    AL1(1,3),C'n/d'                                                  
*                                                                               
DDTXTX   DC    AL2(65535)                                                       
         EJECT                                                                  
       ++INCLUDE GEDDEQUS                                                       
         SPACE 1                                                                
PANACEA  EQU   24                                                               
         SPACE 1                                                                
         DC    ((((((*-BASE)/0512)+1)*0512)-PANACEA)-(*-BASE))X'00'             
BASEX    DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GEDDENG   01/17/91'                                      
         END                                                                    
