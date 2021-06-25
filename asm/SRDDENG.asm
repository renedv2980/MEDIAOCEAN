*          DATA SET SRDDENG    AT LEVEL 002 AS OF 08/22/00                      
*PHASE T00D10A                                                                  
         TITLE 'SVC SYSTEM - DATA DICTIONARY - ENGLISH'                         
SRDDENG  CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'SRDDENG '                                                    
         DC    AL4(BASEX-BASE-PANACEA)                                          
         DC    AL2((DDNDXX-DDNDX-2)/2)                                          
         DC    AL2(0)                                                           
         SPACE 2                                                                
DDNDX    DC    AL2(0)                                                           
*                                                                               
         DC    AL2(DD0001-BASE)    page                                         
         DC    AL2(DD0002-BASE)    pages                                        
         DC    AL2(DD0003-BASE)    characters                                   
         DC    AL2(DD0004-BASE)    found                                        
         DC    AL2(DD0005-BASE)    buffer                                       
         DC    AL2(DD0006-BASE)    find                                         
         DC    AL2(DD0007-BASE)    right                                        
         DC    AL2(DD0008-BASE)    left                                         
         DC    AL2(DD0009-BASE)    linedown                                     
*                                                                               
         DC    AL2(DD0010-BASE)    lineup                                       
         DC    AL2(DD0011-BASE)    spool id                                     
         DC    AL2(DD0012-BASE)    line                                         
         DC    AL2(DD0013-BASE)    column                                       
         DC    AL2(DD0014-BASE)    option                                       
         DC    AL2(DD0015-BASE)    scroll                                       
         DC    AL2(DD0016-BASE)    pageup                                       
         DC    AL2(DD0017-BASE)    pagedown                                     
         DC    AL2(DD0018-BASE)    submitted                                    
         DC    AL2(DD0019-BASE)    submit                                       
*                                                                               
         DC    AL2(DD0020-BASE)    all                                          
         DC    AL2(DD0021-BASE)    change                                       
         DC    AL2(DD0022-BASE)    date=today                                   
         DC    AL2(DD0023-BASE)    lu id     (logical unit id)                  
         DC    AL2(DD0024-BASE)    notified                                     
         DC    AL2(DD0025-BASE)    hold                                         
         DC    AL2(DD0026-BASE)    active                                       
         DC    AL2(DD0027-BASE)    sent                                         
         DC    AL2(DD0028-BASE)    printed                                      
         DC    AL2(DD0029-BASE)    node                                         
*                                                                               
         DC    AL2(DD0030-BASE)    start                                        
         DC    AL2(DD0031-BASE)    display                                      
         DC    AL2(DD0032-BASE)    printer                                      
         DC    AL2(DD0033-BASE)    shuttle                                      
         DC    AL2(DD0034-BASE)    auto                                         
         DC    AL2(DD0035-BASE)    inactive                                     
         DC    AL2(DD0036-BASE)    data                                         
         DC    AL2(DD0037-BASE)    stopped                                      
         DC    AL2(DD0038-BASE)    due to error                                 
         DC    AL2(DD0039-BASE)    printing                                     
*                                                                               
         DC    AL2(DD0040-BASE)    sending                                      
         DC    AL2(DD0041-BASE)    at page                                      
         DC    AL2(DD0042-BASE)    started                                      
         DC    AL2(DD0043-BASE)    report purged/expired                        
         DC    AL2(DD0044-BASE)    language                                     
         DC    AL2(DD0045-BASE)    country                                      
         DC    AL2(DD0046-BASE)    status displayed                             
         DC    AL2(DD0047-BASE)    release                                      
         DC    AL2(DD0048-BASE)    back                                         
         DC    AL2(DD0049-BASE)    scheduled for stop                           
*                                                                               
         DC    AL2(DD0050-BASE)    status and queue displayed                   
         DC    AL2(DD0051-BASE)    new status                                   
         DC    AL2(DD0052-BASE)    lines                                        
         DC    AL2(DD0053-BASE)    print                                        
         DC    AL2(DD0054-BASE)    time                                         
         DC    AL2(DD0055-BASE)    report type                                  
         DC    AL2(DD0056-BASE)    report filter                                
         DC    AL2(DD0057-BASE)    permanent                                    
         DC    AL2(DD0058-BASE)    zero                                         
         DC    AL2(DD0059-BASE)    hours                                        
*                                                                               
         DC    AL2(DD0060-BASE)    report displayed                             
         DC    AL2(DD0061-BASE)    report purged                                
         DC    AL2(DD0062-BASE)    compact                                      
         DC    AL2(DD0063-BASE)    middle                                       
         DC    AL2(DD0064-BASE)    centre                                       
         DC    AL2(DD0065-BASE)    user id                                      
         DC    AL2(DD0066-BASE)    count                                        
         DC    AL2(DD0067-BASE)    first                                        
         DC    AL2(DD0068-BASE)    userids                                      
         DC    AL2(DD0069-BASE)    running                                      
*                                                                               
         DC    AL2(DD0070-BASE)    next                                         
         DC    AL2(DD0071-BASE)    all reports cleared                          
         DC    AL2(DD0072-BASE)    keep                                         
         DC    AL2(DD0073-BASE)    total                                        
         DC    AL2(DD0074-BASE)    seq#                                         
         DC    AL2(DD0075-BASE)    scheduled                                    
         DC    AL2(DD0076-BASE)    list format                                  
         DC    AL2(DD0077-BASE)    page,line                                    
         DC    AL2(DD0078-BASE)    of                                           
         DC    AL2(DD0079-BASE)    thru                                         
*                                                                               
         DC    AL2(DD0080-BASE)    report count                                 
         DC    AL2(DD0081-BASE)    report id                                    
         DC    AL2(DD0082-BASE)    expiry date                                  
         DC    AL2(DD0083-BASE)    report class                                 
         DC    AL2(DD0084-BASE)    expiry time                                  
         DC    AL2(DD0085-BASE)    report name                                  
         DC    AL2(DD0086-BASE)    high                                         
         DC    AL2(DD0087-BASE)    status                                       
         DC    AL2(DD0088-BASE)    date created                                 
         DC    AL2(DD0089-BASE)    number of pages                              
*                                                                               
         DC    AL2(DD0090-BASE)    time created                                 
         DC    AL2(DD0091-BASE)    number of lines                              
         DC    AL2(DD0092-BASE)    active retain                                
         DC    AL2(DD0093-BASE)    printed location                             
         DC    AL2(DD0094-BASE)    printed date                                 
         DC    AL2(DD0095-BASE)    printed count                                
         DC    AL2(DD0096-BASE)    printed time                                 
         DC    AL2(DD0097-BASE)    printed device                               
         DC    AL2(DD0098-BASE)    printed retain                               
         DC    AL2(DD0099-BASE)    line type                                    
*                                                                               
         DC    AL2(DD0100-BASE)    number of control intervals                  
         DC    AL2(DD0101-BASE)    lines/page                                   
         DC    AL2(DD0102-BASE)    password                                     
         DC    AL2(DD0103-BASE)    average cpl                                  
         DC    AL2(DD0104-BASE)    maximum cpl                                  
         DC    AL2(DD0105-BASE)    action                                       
         DC    AL2(DD0106-BASE)    printer id                                   
         DC    AL2(DD0107-BASE)    printer option                               
         DC    AL2(DD0108-BASE)    printer status                               
         DC    AL2(DD0109-BASE)    status changes                               
*                                                                               
         DC    AL2(DD0110-BASE)    alpha                                        
         DC    AL2(DD0111-BASE)    reports                                      
         DC    AL2(DD0112-BASE)    live hours                                   
         DC    AL2(DD0113-BASE)    job                                          
         DC    AL2(DD0114-BASE)    class                                        
         DC    AL2(DD0115-BASE)    description                                  
         DC    AL2(DD0116-BASE)    forms                                        
         DC    AL2(DD0117-BASE)    copies                                       
         DC    AL2(DD0118-BASE)    previous                                     
         DC    AL2(DD0119-BASE)    report                                       
*                                                                               
         DC    AL2(DD0120-BASE)    listed                                       
         DC    AL2(DD0121-BASE)    end of report                                
         DC    AL2(DD0122-BASE)    format                                       
         DC    AL2(DD0123-BASE)    connect status                               
         DC    AL2(DD0124-BASE)    terminal                                     
         DC    AL2(DD0125-BASE)    number                                       
         DC    AL2(DD0126-BASE)    id                                           
         DC    AL2(DD0127-BASE)    system                                       
         DC    AL2(DD0128-BASE)    address                                      
         DC    AL2(DD0129-BASE)    type                                         
*                                                                               
         DC    AL2(DD0130-BASE)    program                                      
         DC    AL2(DD0131-BASE)    id number                                    
         DC    AL2(DD0132-BASE)    screen                                       
         DC    AL2(DD0133-BASE)    control unit                                 
         DC    AL2(DD0134-BASE)    yes                                          
         DC    AL2(DD0135-BASE)    select                                       
         DC    AL2(DD0136-BASE)    line(s)  (transmission)                      
         DC    AL2(DD0137-BASE)    office code                                  
         DC    AL2(DD0138-BASE)    line     (transmission)                      
         DC    AL2(DD0139-BASE)    local                                        
*                                                                               
         DC    AL2(DD0140-BASE)    ready                                        
         DC    AL2(DD0141-BASE)    help                                         
         DC    AL2(DD0142-BASE)    dial                                         
         DC    AL2(DD0143-BASE)    mdrop                                        
         DC    AL2(DD0144-BASE)    lease                                        
         DC    AL2(DD0145-BASE)    not open                                     
         DC    AL2(DD0146-BASE)    up & open                                    
         DC    AL2(DD0147-BASE)    down & open                                  
         DC    AL2(DD0148-BASE)    device address                               
         DC    AL2(DD0149-BASE)    held                                         
*                                                                               
         DC    AL2(DD0150-BASE)    access code                                  
         DC    AL2(DD0151-BASE)    update                                       
         DC    AL2(DD0152-BASE)    no                                           
         DC    AL2(DD0153-BASE)    log                                          
         DC    AL2(DD0154-BASE)    ok                                           
         DC    AL2(DD0155-BASE)    unknown                                      
         DC    AL2(DD0156-BASE)    sys input num                                
         DC    AL2(DD0157-BASE)    first word                                   
         DC    AL2(DD0158-BASE)    second word                                  
         DC    AL2(DD0159-BASE)    reverse mask                                 
*                                                                               
         DC    AL2(DD0160-BASE)    connect                                      
         DC    AL2(DD0161-BASE)    violate                                      
         DC    AL2(DD0162-BASE)    message number                               
         DC    AL2(DD0163-BASE)    terminal id                                  
         DC    AL2(DD0164-BASE)    line(s)                                      
         DC    AL2(DD0165-BASE)    confirm                                      
         DC    AL2(DD0166-BASE)    text                                         
         DC    AL2(DD0167-BASE)    maximum                                      
         DC    AL2(DD0168-BASE)    public                                       
         DC    AL2(DD0169-BASE)    end                                          
*                                                                               
         DC    AL2(DD0170-BASE)    queue                                        
         DC    AL2(DD0171-BASE)    stop                                         
         DC    AL2(DD0172-BASE)    last                                         
         DC    AL2(DD0173-BASE)    schedule                                     
         DC    AL2(DD0174-BASE)    error                                        
         DC    AL2(DD0175-BASE)    flush                                        
         DC    AL2(DD0176-BASE)    user                                         
         DC    AL2(DD0177-BASE)    alter                                        
         DC    AL2(DD0178-BASE)    date                                         
         DC    AL2(DD0179-BASE)    kill                                         
*                                                                               
         DC    AL2(DD0180-BASE)    mode                                         
         DC    AL2(DD0181-BASE)    today                                        
         DC    AL2(DD0182-BASE)    purge                                        
         DC    AL2(DD0183-BASE)    activate                                     
         DC    AL2(DD0184-BASE)    unkeep                                       
         DC    AL2(DD0185-BASE)    clear                                        
         DC    AL2(DD0186-BASE)    size                                         
         DC    AL2(DD0187-BASE)    retain                                       
         DC    AL2(DD0188-BASE)    list                                         
         DC    AL2(DD0189-BASE)    manual                                       
*                                                                               
         DC    AL2(DD0190-BASE)    temporary                                    
         DC    AL2(DD0191-BASE)    top                                          
         DC    AL2(DD0192-BASE)    delete                                       
         DC    AL2(DD0193-BASE)    line id                                      
         DC    AL2(DD0194-BASE)    purged                                       
         DC    AL2(DD0195-BASE)    live                                         
         DC    AL2(DD0196-BASE)    dead                                         
         DC    AL2(DD0197-BASE)    wide                                         
         DC    AL2(DD0198-BASE)    sort                                         
         DC    AL2(DD0199-BASE)    cdate                                        
*                                                                               
         DC    AL2(DD0200-BASE)    pdate                                        
         DC    AL2(DD0201-BASE)    sdate                                        
         DC    AL2(DD0202-BASE)    rdate                                        
         DC    AL2(DD0203-BASE)    dds                                          
         DC    AL2(DD0204-BASE)    created                                      
         DC    AL2(DD0205-BASE)    bottom                                       
         DC    AL2(DD0206-BASE)    search id                                    
         DC    AL2(DD0207-BASE)    filters                                      
         DC    AL2(DD0208-BASE)    return                                       
         DC    AL2(DD0209-BASE)    no item                                      
*                                                                               
         DC    AL2(DD0210-BASE)    invalid item                                 
         DC    AL2(DD0211-BASE)    item                                         
         DC    AL2(DD0212-BASE)    search                                       
         DC    AL2(DD0213-BASE)    qualifier                                    
         DC    AL2(DD0214-BASE)    list of                                      
*                                                                               
DDNDXX   DC    AL2(65535)                                                       
         SPACE 2                                                                
DDTXT    DS    0C                                                               
*                                                                               
DD0001   DC    AL1(1,04),C'page'                                                
DD0002   DC    AL1(1,05),C'pages'                                               
DD0003   DC    AL1(2,4,10),C'chrs',C'characters'                                
DD0004   DC    AL1(1,05),C'found'                                               
DD0005   DC    AL1(1,06),C'buffer'                                              
DD0006   DC    AL1(1,04),C'find'                                                
DD0007   DC    AL1(1,05),C'right'                                               
DD0008   DC    AL1(1,04),C'left '                                               
DD0009   DC    AL1(1,08),C'linedown '                                           
*                                                                               
DD0010   DC    AL1(1,06),C'lineup'                                              
DD0011   DC    AL1(1,08),C'spool id'                                            
DD0012   DC    AL1(1,04),C'line'                                                
DD0013   DC    AL1(1,06),C'column'                                              
DD0014   DC    AL1(1,06),C'option'                                              
DD0015   DC    AL1(1,06),C'scroll'                                              
DD0016   DC    AL1(1,06),C'pageup'                                              
DD0017   DC    AL1(1,08),C'pagedown'                                            
DD0018   DC    AL1(1,09),C'submitted'                                           
DD0019   DC    AL1(1,06),C'submit'                                              
*                                                                               
DD0020   DC    AL1(1,03),C'all'                                                 
DD0021   DC    AL1(1,06),C'change'                                              
DD0022   DC    AL1(1,10),C'date=today'                                          
DD0023   DC    AL1(1,05),C'LU Id'                                               
DD0024   DC    AL1(1,08),C'notified'                                            
DD0025   DC    AL1(2,3,04),C'hld',C'hold'                                       
DD0026   DC    AL1(2,04,06),C'actv',C'active'                                   
DD0027   DC    AL1(2,3,04),C'snt',C'sent'                                       
DD0028   DC    AL1(3,3,04,07),C'ptd',C'prtd',C'printed'                         
DD0029   DC    AL1(1,04),C'Node'                                                
*                                                                               
DD0030   DC    AL1(2,4,5),C'strt',C'start'                                      
DD0031   DC    AL1(2,4,7),C'disp',C'display'                                    
DD0032   DC    AL1(1,07),C'printer'                                             
DD0033   DC    AL1(1,07),C'shuttle'                                             
DD0034   DC    AL1(1,04),C'auto'                                                
DD0035   DC    AL1(1,08),C'inactive'                                            
DD0036   DC    AL1(1,04),C'data'                                                
DD0037   DC    AL1(1,07),C'stopped'                                             
DD0038   DC    AL1(1,12),C'due to error'                                        
DD0039   DC    AL1(3,2,04,08),C'g ',C'pntg',C'printing'                         
*                                                                               
DD0040   DC    AL1(1,07),C'sending'                                             
DD0041   DC    AL1(1,07),C'at page'                                             
DD0042   DC    AL1(1,07),C'started'                                             
DD0043   DC    AL1(1,34),C'***** Report Purged / Expired ****'                  
DD0044   DC    AL1(1,08),C'language'                                            
DD0045   DC    AL1(1,07),C'country'                                             
DD0046   DC    AL1(1,16),C'status displayed'                                    
DD0047   DC    AL1(1,07),C'release'                                             
DD0048   DC    AL1(1,4),C'back'                                                 
DD0049   DC    AL1(1,18),C'scheduled for stop'                                  
*                                                                               
DD0050   DC    AL1(1,26),C'status and queue displayed'                          
DD0051   DC    AL1(1,10),C'new status'                                          
DD0052   DC    AL1(1,05),C'lines'                                               
DD0053   DC    AL1(1,05),C'print'                                               
DD0054   DC    AL1(1,04),C'time'                                                
DD0055   DC    AL1(1,11),C'report type'                                         
DD0056   DC    AL1(2,11,13),C'report filt',C'report filter'                     
DD0057   DC    AL1(1,09),C'permanent'                                           
DD0058   DC    AL1(1,04),C'zero'                                                
DD0059   DC    AL1(2,3,5),C'hrs',C'hours'                                       
*                                                                               
DD0060   DC    AL1(1,16),C'report displayed'                                    
DD0061   DC    AL1(1,13),C'report purged'                                       
DD0062   DC    AL1(1,07),C'compact'                                             
DD0063   DC    AL1(1,06),C'middle'                                              
DD0064   DC    AL1(1,06),C'centre'                                              
DD0065   DC    AL1(1,07),C'user id'                                             
DD0066   DC    AL1(1,05),C'count'                                               
DD0067   DC    AL1(1,05),C'first'                                               
DD0068   DC    AL1(1,07),C'userids'                                             
DD0069   DC    AL1(1,7),C'running'                                              
*                                                                               
DD0070   DC    AL1(1,4),C'next'                                                 
DD0071   DC    AL1(1,19),C'all reports cleared'                                 
DD0072   DC    AL1(1,04),C'keep'                                                
DD0073   DC    AL1(2,4,5),C'totl',C'total'                                      
DD0074   DC    AL1(1,04),C'seq#'                                                
DD0075   DC    AL1(1,09),C'scheduled'                                           
DD0076   DC    AL1(1,11),C'list format'                                         
DD0077   DC    AL1(1,09),C'page,line'                                           
DD0078   DC    AL1(1,02),C'of'                                                  
DD0079   DC    AL1(1,04),C'thru'                                                
*                                                                               
DD0080   DC    AL1(1,12),C'report count'                                        
DD0081   DC    AL1(2,08,09),C'reportid',C'report id'                            
DD0082   DC    AL1(1,11),C'expiry date'                                         
DD0083   DC    AL1(1,12),C'report class'                                        
DD0084   DC    AL1(1,11),C'expiry time'                                         
DD0085   DC    AL1(1,11),C'report name'                                         
DD0086   DC    AL1(1,4),C'high'                                                 
DD0087   DC    AL1(1,06),C'status'                                              
DD0088   DC    AL1(1,12),C'date created'                                        
DD0089   DC    AL1(3,9,12,15),C'num pages',C'num of pages'                      
         DC             C'number of pages'                                      
*                                                                               
DD0090   DC    AL1(1,12),C'time created'                                        
DD0091   DC    AL1(3,9,12,15),C'num lines',C'num of lines'                      
         DC             C'number of lines'                                      
DD0092   DC    AL1(2,11,13),C'actv retain',C'active retain'                     
DD0093   DC    AL1(2,09,16),C'prtd locn',C'printed location'                    
DD0094   DC    AL1(2,09,12),C'prtd date',C'printed date'                        
DD0095   DC    AL1(2,10,13),C'prtd count',C'printed count'                      
DD0096   DC    AL1(2,09,12),C'prtd time',C'printed time'                        
DD0097   DC    AL1(2,11,14),C'prtd device',C'printed device'                    
DD0098   DC    AL1(2,11,14),C'prtd retain',C'printed retain'                    
DD0099   DC    AL1(1,09),C'line type'                                           
*                                                                               
DD0100   DC    AL1(2,10,21),C'num of cis',C'number of ctrl intvls'              
DD0101   DC    AL1(1,10),C'lines/page'                                          
DD0102   DC    AL1(1,08),C'password'                                            
DD0103   DC    AL1(1,11),C'average cpl'                                         
DD0104   DC    AL1(1,11),C'maximum cpl'                                         
DD0105   DC    AL1(1,06),C'action'                                              
DD0106   DC    AL1(1,10),C'printer id'                                          
DD0107   DC    AL1(2,12,14),C'printer optn',C'printer option'                   
DD0108   DC    AL1(1,14),C'printer status'                                      
DD0109   DC    AL1(1,14),C'status changes'                                      
*                                                                               
DD0110   DC    AL1(1,05),C'alpha'                                               
DD0111   DC    AL1(2,04,07),C'rpts',C'reports'                                  
DD0112   DC    AL1(1,10),C'live hours'                                          
DD0113   DC    AL1(1,3),C'job'                                                  
DD0114   DC    AL1(1,05),C'class'                                               
DD0115   DC    AL1(1,11),C'description'                                         
DD0116   DC    AL1(1,05),C'forms'                                               
DD0117   DC    AL1(2,5,6),C'copys',C'copies'                                    
DD0118   DC    AL1(1,8),C'previous'                                             
DD0119   DC    AL1(1,06),C'report'                                              
*                                                                               
DD0120   DC    AL1(1,06),C'listed'                                              
DD0121   DC    AL1(1,13),C'end of report'                                       
DD0122   DC    AL1(2,4,6),C'frmt',C'format'                                     
DD0123   DC    AL1(1,14),C'connect status'                                      
DD0124   DC    AL1(3,3,4,8),C'trm',C'term',C'terminal'                          
DD0125   DC    AL1(1,06),C'number'                                              
DD0126   DC    AL1(1,02),C'id'                                                  
DD0127   DC    AL1(1,06),C'system'                                              
DD0128   DC    AL1(1,07),C'address'                                             
DD0129   DC    AL1(1,04),C'type'                                                
*                                                                               
DD0130   DC    AL1(3,3,4,7),C'pgm',C'prog',C'program'                           
DD0131   DC    AL1(1,09),C'id number'                                           
DD0132   DC    AL1(2,3,6),C'scr',C'screen'                                      
DD0133   DC    AL1(1,10),C'Cntrl unit'                                          
DD0134   DC    AL1(1,03),C'yes'                                                 
DD0135   DC    AL1(1,06),C'select'                                              
DD0136   DC    AL1(1,07),C'line(s)'                                             
DD0137   DC    AL1(1,11),C'office code'                                         
DD0138   DC    AL1(1,04),C'line'                                                
DD0139   DC    AL1(1,05),C'local'                                               
*                                                                               
DD0140   DC    AL1(1,05),C'ready'                                               
DD0141   DC    AL1(1,04),C'help'                                                
DD0142   DC    AL1(1,04),C'dial'                                                
DD0143   DC    AL1(1,05),C'mdrop'                                               
DD0144   DC    AL1(1,05),C'lease'                                               
DD0145   DC    AL1(1,08),C'not open'                                            
DD0146   DC    AL1(1,09),C'up && open'                                          
DD0147   DC    AL1(1,11),C'down && open'                                        
DD0148   DC    AL1(1,11),C'Device addr'                                         
DD0149   DC    AL1(1,04),C'held'                                                
*                                                                               
DD0150   DC    AL1(1,11),C'access code'                                         
DD0151   DC    AL1(1,06),C'update'                                              
DD0152   DC    AL1(1,02),C'no'                                                  
DD0153   DC    AL1(1,03),C'log'                                                 
DD0154   DC    AL1(1,03),C'ok '                                                 
DD0155   DC    AL1(1,07),C'unknown'                                             
DD0156   DC    AL1(1,13),C'Sys Input No.'                                       
DD0157   DC    AL1(1,10),C'first word'                                          
DD0158   DC    AL1(1,11),C'second word'                                         
DD0159   DC    AL1(1,12),C'reverse mask'                                        
*                                                                               
DD0160   DC    AL1(1,07),C'connect'                                             
DD0161   DC    AL1(1,07),C'violate'                                             
DD0162   DC    AL1(2,7,14),C'msg num',C'message number'                         
DD0163   DC    AL1(2,7,11),C'term id',C'terminal id'                            
DD0164   DC    AL1(1,07),C'line(s)'                                             
DD0165   DC    AL1(1,07),C'confirm'                                             
DD0166   DC    AL1(1,04),C'text'                                                
DD0167   DC    AL1(2,03,07),C'max',C'maximum'                                   
DD0168   DC    AL1(1,06),C'public'                                              
DD0169   DC    AL1(1,03),C'end'                                                 
*                                                                               
DD0170   DC    AL1(1,05),C'queue'                                               
DD0171   DC    AL1(1,04),C'stop'                                                
DD0172   DC    AL1(1,04),C'last'                                                
DD0173   DC    AL1(2,4,08),C'schd',C'schedule'                                  
DD0174   DC    AL1(1,5),C'error'                                                
DD0175   DC    AL1(2,4,5),C'flsh',C'flush'                                      
DD0176   DC    AL1(1,04),C'user'                                                
DD0177   DC    AL1(1,05),C'alter'                                               
DD0178   DC    AL1(1,04),C'date'                                                
DD0179   DC    AL1(1,04),C'kill'                                                
*                                                                               
DD0180   DC    AL1(1,04),C'mode'                                                
DD0181   DC    AL1(1,05),C'today'                                               
DD0182   DC    AL1(1,05),C'purge'                                               
DD0183   DC    AL1(2,4,8),C'actv',C'activate'                                   
DD0184   DC    AL1(1,06),C'unkeep'                                              
DD0185   DC    AL1(1,05),C'clear'                                               
DD0186   DC    AL1(1,04),C'size'                                                
DD0187   DC    AL1(2,4,6),C'retn',C'retain'                                     
DD0188   DC    AL1(1,04),C'list'                                                
DD0189   DC    AL1(1,06),C'manual'                                              
*                                                                               
DD0190   DC    AL1(1,09),C'temporary'                                           
DD0191   DC    AL1(1,03),C'top'                                                 
DD0192   DC    AL1(1,06),C'delete'                                              
DD0193   DC    AL1(1,07),C'Line Id'                                             
DD0194   DC    AL1(1,06),C'purged'                                              
DD0195   DC    AL1(1,04),C'live'                                                
DD0196   DC    AL1(1,04),C'dead'                                                
DD0197   DC    AL1(1,04),C'wide'                                                
DD0198   DC    AL1(1,04),C'sort'                                                
DD0199   DC    AL1(1,05),C'cdate'                                               
*                                                                               
DD0200   DC    AL1(1,05),C'pdate'                                               
DD0201   DC    AL1(1,05),C'sdate'                                               
DD0202   DC    AL1(1,05),C'rdate'                                               
DD0203   DC    AL1(1,03),C'dds'                                                 
DD0204   DC    AL1(1,07),C'created'                                             
DD0205   DC    AL1(1,06),C'bottom'                                              
DD0206   DC    AL1(1,09),C'Search Id'                                           
DD0207   DC    AL1(1,07),C'Filters'                                             
DD0208   DC    AL1(1,06),C'Return'                                              
DD0209   DC    AL1(1,07),C'No Item'                                             
*                                                                               
DD0210   DC    AL1(1,12),C'Invalid Item'                                        
DD0211   DC    AL1(1,04),C'Item'                                                
DD0212   DC    AL1(1,06),C'Search'                                              
DD0213   DC    AL1(1,09),C'Qualifier'                                           
DD0214   DC    AL1(1,07),C'List of'                                             
*                                                                               
DDTXTX   DC    AL2(65535)                                                       
         EJECT                                                                  
       ++INCLUDE SRDDEQUS                                                       
         SPACE 1                                                                
PANACEA  EQU   24                                                               
         SPACE 1                                                                
         DC    ((((((*-BASE)/0512)+1)*0512)-PANACEA)-(*-BASE))X'00'             
BASEX    DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRDDENG   08/22/00'                                      
         END                                                                    
