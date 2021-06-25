*          DATA SET SPDDENG    AT LEVEL 050 AS OF 12/09/92                      
*PHASE T00D20A                                                                  
         TITLE 'SPOT SYSTEM - DATA DICTIONARY - ENGLISH'                        
SPDDENG  CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'SPDDENG '                                                    
         DC    AL4(BASEX-BASE+PANACEA)                                          
         DC    AL2((DDNDXX-DDNDX-2)/2)                                          
         DC    AL2(0)                                                           
         SPACE 2                                                                
DDNDX    DC    AL2(0)                                                           
*                                                                               
         DC    AL2(DD0001-BASE)    record type                                  
         DC    AL2(DD0002-BASE)    action                                       
         DC    AL2(DD0003-BASE)    key                                          
         DC    AL2(DD0004-BASE)    print                                        
         DC    AL2(DD0005-BASE)    output                                       
         DC    AL2(DD0006-BASE)    destination                                  
         DC    AL2(DD0007-BASE)    others                                       
         DC    AL2(DD0008-BASE)    filters                                      
         DC    AL2(DD0009-BASE)    options                                      
*                                                                               
         DC    AL2(DD0010-BASE)    agency                                       
         DC    AL2(DD0011-BASE)    agency name                                  
         DC    AL2(DD0012-BASE)    media                                        
         DC    AL2(DD0013-BASE)    media name                                   
         DC    AL2(DD0014-BASE)    client                                       
         DC    AL2(DD0015-BASE)    client name                                  
         DC    AL2(DD0016-BASE)    product                                      
         DC    AL2(DD0017-BASE)    product name                                 
         DC    AL2(DD0018-BASE)    date                                         
         DC    AL2(DD0019-BASE)    period                                       
*                                                                               
         DC    AL2(DD0020-BASE)    run on                                       
         DC    AL2(DD0021-BASE)    at                                           
         DC    AL2(DD0022-BASE)    report                                       
         DC    AL2(DD0023-BASE)    page                                         
         DC    AL2(DD0024-BASE)    requestor                                    
         DC    AL2(DD0025-BASE)    time                                         
         DC    AL2(DD0026-BASE)    n/d                                          
         DC    AL2(DD0027-BASE)    not used                                     
         DC    AL2(DD0028-BASE)    not used                                     
         DC    AL2(DD0029-BASE)    not used                                     
*                                                                               
         DC    AL2(DD0030-BASE)    combined tv                                  
         DC    AL2(DD0031-BASE)    coverage                                     
         DC    AL2(DD0032-BASE)    network tv                                   
         DC    AL2(DD0033-BASE)    ***spill***                                  
         DC    AL2(DD0034-BASE)    spot tv                                      
         DC    AL2(DD0035-BASE)    **film**                                     
         DC    AL2(DD0036-BASE)    ***tax excluded***                           
         DC    AL2(DD0037-BASE)    pnts                                         
         DC    AL2(DD0038-BASE)    cpp                                          
         DC    AL2(DD0039-BASE)    imps                                         
*                                                                               
         DC    AL2(DD0040-BASE)    cpm                                          
         DC    AL2(DD0041-BASE)    goal                                         
         DC    AL2(DD0042-BASE)    purchased on, purchased,purch                
         DC    AL2(DD0043-BASE)    achieved                                     
         DC    AL2(DD0044-BASE)    affidavit                                    
         DC    AL2(DD0045-BASE)    order,orderd                                 
         DC    AL2(DD0046-BASE)    subpage                                      
         DC    AL2(DD0047-BASE)    from                                         
         DC    AL2(DD0048-BASE)    to                                           
         DC    AL2(DD0049-BASE)    estimate                                     
*                                                                               
         DC    AL2(DD0050-BASE)    all estimates                                
         DC    AL2(DD0051-BASE)    est fltr                                     
         DC    AL2(DD0052-BASE)    estimate group                               
         DC    AL2(DD0053-BASE)    all                                          
         DC    AL2(DD0054-BASE)    markets,market,mrkt                          
         DC    AL2(DD0055-BASE)    station                                      
         DC    AL2(DD0056-BASE)    all dayparts                                 
         DC    AL2(DD0057-BASE)    all spot lengths                             
         DC    AL2(DD0058-BASE)    nsi                                          
         DC    AL2(DD0059-BASE)    arb                                          
*                                                                               
         DC    AL2(DD0060-BASE)    bbm                                          
         DC    AL2(DD0061-BASE)    affid on                                     
         DC    AL2(DD0062-BASE)    actual                                       
         DC    AL2(DD0063-BASE)    var.                                         
         DC    AL2(DD0064-BASE)    latest                                       
         DC    AL2(DD0065-BASE)    book                                         
         DC    AL2(DD0066-BASE)    auto                                         
         DC    AL2(DD0067-BASE)    adj                                          
         DC    AL2(DD0068-BASE)    based on affidavit                           
         DC    AL2(DD0069-BASE)    equivalence base                             
*                                                                               
         DC    AL2(DD0070-BASE)    sec. (+)                                     
         DC    AL2(DD0071-BASE)    group                                        
         DC    AL2(DD0072-BASE)    (goal vs. purchased)                         
         DC    AL2(DD0073-BASE)    (goal vs. affidavit)                         
         DC    AL2(DD0074-BASE)    (purchased vs. rerated)                      
         DC    AL2(DD0075-BASE)    (purchased vs. affidavit)                    
         DC    AL2(DD0076-BASE)    (lockin vs. purchased)                       
         DC    AL2(DD0077-BASE)    (lockin vs. affidavit)                       
         DC    AL2(DD0078-BASE)    (goal vs. lockin)                            
         DC    AL2(DD0079-BASE)    (goal vs. purchased vs. rerated)             
*                                                                               
         DC    AL2(DD0080-BASE)    (goal vs. purchased vs. affidavit)           
         DC    AL2(DD0081-BASE)    (goal vs. lockin vs. rerated)                
         DC    AL2(DD0082-BASE)    (goal vs. lockin vs. affidavit)              
         DC    AL2(DD0083-BASE)    (rating source)                              
         DC    AL2(DD0084-BASE)    sp@@ on                                      
         DC    AL2(DD0085-BASE)    ave  pct                                     
         DC    AL2(DD0086-BASE)    achmt                                        
         DC    AL2(DD0087-BASE)    daypart-ln                                   
         DC    AL2(DD0088-BASE)    dollars,dols                                 
         DC    AL2(DD0089-BASE)    spot,spots,*spot*                            
*                                                                               
         DC    AL2(DD0090-BASE)    primary dem                                  
         DC    AL2(DD0091-BASE)    ----goal----                                 
         DC    AL2(DD0092-BASE)    ----purchased---                             
         DC    AL2(DD0093-BASE)    ----goal(                                    
         DC    AL2(DD0094-BASE)    pts  pnts                                    
         DC    AL2(DD0095-BASE)    imps  imps                                   
         DC    AL2(DD0096-BASE)    pcnt                                         
         DC    AL2(DD0097-BASE)    (a),(auto),(automatic)                       
         DC    AL2(DD0098-BASE)    not used                                     
         DC    AL2(DD0099-BASE)    *** all brands ***                           
*                                                                               
         DC    AL2(DD0100-BASE)    brands                                       
         DC    AL2(DD0101-BASE)    affd-reg                                     
         DC    AL2(DD0102-BASE)    spcl                                         
         DC    AL2(DD0103-BASE)    continued                                    
         DC    AL2(DD0104-BASE)    demo                                         
         DC    AL2(DD0105-BASE)    ***m o n t h l y***                          
         DC    AL2(DD0106-BASE)    **q u a r t e r l y**                        
         DC    AL2(DD0107-BASE)    ***custom quarters***                        
         DC    AL2(DD0108-BASE)    prd(target)                                  
         DC    AL2(DD0109-BASE)    pcnt pcnt                                    
*                                                                               
         DC    AL2(DD0110-BASE)    demo dols,demo dollars                       
         DC    AL2(DD0111-BASE)    cpp/m                                        
         DC    AL2(DD0112-BASE)    * market performance *                       
         DC    AL2(DD0113-BASE)    --demographics--                             
         DC    AL2(DD0114-BASE)    dem  demo                                    
         DC    AL2(DD0115-BASE)    market performance report                    
         DC    AL2(DD0116-BASE)    brand weekly summary                         
         DC    AL2(DD0117-BASE)    week of                                      
         DC    AL2(DD0118-BASE)    tot,total,*total*,**total**                  
         DC    AL2(DD0119-BASE)    ***special rep=@@@***                        
*                                                                               
         DC    AL2(DD0120-BASE)    *** summary ****                             
         DC    AL2(DD0121-BASE)    pkg mst                                      
         DC    AL2(DD0122-BASE)    orb mst                                      
         DC    AL2(DD0123-BASE)    rev mst                                      
         DC    AL2(DD0124-BASE)    *mg*                                         
         DC    AL2(DD0125-BASE)    mst=                                         
         DC    AL2(DD0126-BASE)    total                                        
         DC    AL2(DD0127-BASE)    tlcsts                                       
         DC    AL2(DD0128-BASE)    brcsts,brdcsts,broadcasts                    
         DC    AL2(DD0129-BASE)    day                                          
*                                                                               
         DC    AL2(DD0130-BASE)    programming                                  
         DC    AL2(DD0131-BASE)    len                                          
         DC    AL2(DD0132-BASE)    rotation schedule                            
         DC    AL2(DD0133-BASE)    network rotation schedule                    
         DC    AL2(DD0134-BASE)    exchange                                     
         DC    AL2(DD0135-BASE)    pol                                          
         DC    AL2(DD0136-BASE)    address missing                              
         DC    AL2(DD0137-BASE)    *orbit*                                      
         DC    AL2(DD0138-BASE)    unallocated spots                            
         DC    AL2(DD0139-BASE)    pool time sheet                              
*                                                                               
         DC    AL2(DD0140-BASE)    pool turnaround                              
         DC    AL2(DD0141-BASE)    buy description                              
         DC    AL2(DD0142-BASE)    rotation pattern                             
         DC    AL2(DD0143-BASE)    demographics (@@@)                           
         DC    AL2(DD0144-BASE)    rated program  book                          
         DC    AL2(DD0145-BASE)    comments                                     
         DC    AL2(DD0146-BASE)    rep,rep-                                     
         DC    AL2(DD0147-BASE)    channel                                      
         DC    AL2(DD0148-BASE)    freq                                         
         DC    AL2(DD0149-BASE)    affiliate                                    
*                                                                               
         DC    AL2(DD0150-BASE)    station rotation schedule                    
         DC    AL2(DD0151-BASE)    ***station total***,sta tot                  
         DC    AL2(DD0152-BASE)    ***market total***,mkt tot                   
         DC    AL2(DD0153-BASE)    prd tot,***product total***                  
         DC    AL2(DD0154-BASE)    no tlcsts                                    
         DC    AL2(DD0155-BASE)    no brcsts,no brdcsts                         
         DC    AL2(DD0156-BASE)    ***orig.                                     
         DC    AL2(DD0157-BASE)    mgr tot                                      
         DC    AL2(DD0158-BASE)    cost                                         
         DC    AL2(DD0159-BASE)    *p*                                          
*                                                                               
         DC    AL2(DD0160-BASE)    wkly avg                                     
         DC    AL2(DD0161-BASE)    goal demo                                    
         DC    AL2(DD0162-BASE)    goal $                                       
         DC    AL2(DD0163-BASE)    ***client total***                           
         DC    AL2(DD0164-BASE)    month                                        
         DC    AL2(DD0165-BASE)    *m*                                          
         DC    AL2(DD0166-BASE)    *h*                                          
         DC    AL2(DD0167-BASE)    *u*                                          
         DC    AL2(DD0168-BASE)    *****product legend*****                     
         DC    AL2(DD0169-BASE)    unknown                                      
*                                                                               
         DC    AL2(DD0170-BASE)    ***traffic desk ***                          
         DC    AL2(DD0171-BASE)    ***spills from                               
         DC    AL2(DD0172-BASE)    partner                                      
         DC    AL2(DD0173-BASE)    not used                                     
         DC    AL2(DD0174-BASE)    not used                                     
         DC    AL2(DD0175-BASE)    not used                                     
         DC    AL2(DD0176-BASE)    not used                                     
         DC    AL2(DD0177-BASE)    ***cost overrides***                         
         DC    AL2(DD0178-BASE)    *****piggybacks*****                         
         DC    AL2(DD0179-BASE)    ch                                           
*                                                                               
         DC    AL2(DD0180-BASE)    brand timesheets                             
         DC    AL2(DD0181-BASE)    number of telecasts                          
         DC    AL2(DD0182-BASE)    demographics(imp)/cpp                        
         DC    AL2(DD0183-BASE)    lng/d/p                                      
         DC    AL2(DD0184-BASE)    salespersons signature                       
         DC    AL2(DD0185-BASE)    confirmation of purchase                     
         DC    AL2(DD0186-BASE)    daily time schedule                          
         DC    AL2(DD0187-BASE)    salespersons time schedule                   
         DC    AL2(DD0188-BASE)    lng                                          
         DC    AL2(DD0189-BASE)    demographics(imp=000)                        
*                                                                               
         DC    AL2(DD0190-BASE)    brand time schedule                          
         DC    AL2(DD0191-BASE)    n/w time                                     
         DC    AL2(DD0192-BASE)    dp                                           
         DC    AL2(DD0193-BASE)    book    lng programming                      
         DC    AL2(DD0194-BASE)    cost   pt pkg                                
         DC    AL2(DD0195-BASE)    demographics(imp=000)/cpp                    
         DC    AL2(DD0196-BASE)    network                                      
         DC    AL2(DD0197-BASE)    errors                                       
         DC    AL2(DD0198-BASE)    column numbers                               
         DC    AL2(DD0199-BASE)    request card 1 image                         
*                                                                               
         DC    AL2(DD0200-BASE)    request card 2 image                         
         DC    AL2(DD0201-BASE)    field not numeric                            
         DC    AL2(DD0202-BASE)    not a valid date                             
         DC    AL2(DD0203-BASE)    estimate filters                             
         DC    AL2(DD0204-BASE)    product group                                
         DC    AL2(DD0205-BASE)    market group                                 
         DC    AL2(DD0206-BASE)    process by id                                
         DC    AL2(DD0207-BASE)    est- group end                               
         DC    AL2(DD0208-BASE)    start date                                   
         DC    AL2(DD0209-BASE)    end date                                     
*                                                                               
         DC    AL2(DD0210-BASE)    demo override active                         
         DC    AL2(DD0211-BASE)    affiliation filter                           
         DC    AL2(DD0212-BASE)    program type filter                          
         DC    AL2(DD0213-BASE)    daypart detail con.                          
         DC    AL2(DD0214-BASE)    daypart menu overide                         
         DC    AL2(DD0215-BASE)    option 1                                     
         DC    AL2(DD0216-BASE)    option 2                                     
         DC    AL2(DD0217-BASE)    option 3                                     
         DC    AL2(DD0218-BASE)    option 4                                     
         DC    AL2(DD0219-BASE)    option 5                                     
*                                                                               
         DC    AL2(DD0220-BASE)    requestor name                               
         DC    AL2(DD0221-BASE)    program loop                                 
         DC    AL2(DD0222-BASE)    completion of report                         
         DC    AL2(DD0223-BASE)    first request processed                      
         DC    AL2(DD0224-BASE)    last request processed                       
         DC    AL2(DD0225-BASE)    valid requests processed                     
         DC    AL2(DD0226-BASE)    requests in error                            
         DC    AL2(DD0227-BASE)    total requests processed                     
         DC    AL2(DD0228-BASE)    normal termination                           
         DC    AL2(DD0229-BASE)    operation exception                          
*                                                                               
         DC    AL2(DD0230-BASE)    protection exception                         
         DC    AL2(DD0231-BASE)    addressing exception                         
         DC    AL2(DD0232-BASE)    data exception                               
         DC    AL2(DD0233-BASE)    summary of requests     page 1               
         DC    AL2(DD0234-BASE)    -------------------     ------               
*                                                                               
         DC    AL2(DD0366-BASE)    Compete record                               
         DC    AL2(DD0367-BASE)    ESB record                                   
         DC    AL2(DD0368-BASE)    EBD record                                   
         DC    AL2(DD0369-BASE)    SID record                                   
*                                                                               
         DC    AL2(DD0370-BASE)    DSID record                                  
         DC    AL2(DD0371-BASE)    Code record                                  
         DC    AL2(DD0372-BASE)    PCD record                                   
         DC    AL2(DD0373-BASE)    SPC record                                   
         DC    AL2(DD0374-BASE)    NVL record                                   
         DC    AL2(DD0375-BASE)    ACNEQ record                                 
         DC    AL2(DD0376-BASE)    MGREQ record                                 
         DC    AL2(DD0377-BASE)    Scheme record                                
         DC    AL2(DD0378-BASE)    Period record                                
         DC    AL2(DD0379-BASE)    Station record                               
*                                                                               
         DC    AL2(DD0380-BASE)    NSID record                                  
         DC    AL2(DD0381-BASE)    Detail record                                
         DC    AL2(DD0382-BASE)    AOR record                                   
         DC    AL2(DD0383-BASE)    MTR record                                   
         DC    AL2(DD0384-BASE)    Buy record                                   
         DC    AL2(DD0385-BASE)    Stalist record                               
         DC    AL2(DD0386-BASE)    Estlist record                               
         DC    AL2(DD0387-BASE)    Elist record                                 
         DC    AL2(DD0388-BASE)    PSR record                                   
         DC    AL2(DD0389-BASE)    ESR record                                   
*                                                                               
         DC    AL2(DD0390-BASE)    MKTCORR record                               
         DC    AL2(DD0391-BASE)    NDI record                                   
         DC    AL2(DD0392-BASE)    SPD record                                   
         DC    AL2(DD0393-BASE)    XRATE record                                 
         DC    AL2(DD0394-BASE)    Sgroup record                                
         DC    AL2(DD0395-BASE)    Sgassign record                              
         DC    AL2(DD0396-BASE)    Sgdef record                                 
         DC    AL2(DD0397-BASE)    Cgroup record                                
         DC    AL2(DD0398-BASE)    Cgassign record                              
         DC    AL2(DD0399-BASE)    Cgdef record                                 
*                                                                               
         DC    AL2(DD0400-BASE)    CLRST record                                 
         DC    AL2(DD0401-BASE)    Master record                                
         DC    AL2(DD0402-BASE)    Address record                               
         DC    AL2(DD0403-BASE)    Market record                                
         DC    AL2(DD0404-BASE)    Rep record                                   
         DC    AL2(DD0405-BASE)    User record                                  
         DC    AL2(DD0406-BASE)    Help record                                  
         DC    AL2(DD0407-BASE)    Add a record                                 
         DC    AL2(DD0408-BASE)    Change record                                
         DC    AL2(DD0409-BASE)    Display record                               
*                                                                               
         DC    AL2(DD0410-BASE)    Delete a record                              
         DC    AL2(DD0401-BASE)    Restore a record                             
         DC    AL2(DD0412-BASE)    List records                                 
         DC    AL2(DD0413-BASE)    Select a record                              
         DC    AL2(DD0414-BASE)    Copy a record                                
         DC    AL2(DD0415-BASE)    Rank a record                                
         DC    AL2(DD0416-BASE)    Transfer a record                            
         DC    AL2(DD0417-BASE)    Pcopy a record                               
         DC    AL2(DD0418-BASE)    Help                                         
*                                                                               
DDNDXX   DC    AL2(65535)                                                       
         SPACE 2                                                                
DDTXT    DS    0C                                                               
*                                                                               
DD0001   DC    AL1(2),AL1(6,11),C'Record',C'Record type'                        
DD0002   DC    AL1(1),AL1(6),C'Action'                                          
DD0003   DC    AL1(1),AL1(3),C'Key'                                             
DD0004   DC    AL1(1),AL1(5),C'Print'                                           
DD0005   DC    AL1(1),AL1(6),C'Output'                                          
DD0006   DC    AL1(2),AL1(4,11),C'Dest',C'Destination'                          
DD0007   DC    AL1(1),AL1(6),C'Others'                                          
DD0008   DC    AL1(2),AL1(7,8),C'Filters',C'Filtered'                           
DD0009   DC    AL1(1),AL1(7),C'Options'                                         
*                                                                               
DD0010   DC    AL1(3),AL1(3,6,11),C'Agy',C'Agency',C'Agency code'               
DD0011   DC    AL1(2),AL1(6,11),C'Agency',C'Agency name'                        
DD0012   DC    AL1(3),AL1(3,5,10),C'Med',C'Media',C'Media code'                 
DD0013   DC    AL1(2),AL1(5,10),C'Media',C'Media name'                          
DD0014   DC    AL1(3),AL1(3,6,11),C'Cli',C'Client',C'Client code'               
DD0015   DC    AL1(2),AL1(6,11),C'Client',C'Client name'                        
DD0016   DC    AL1(3),AL1(3,7,12),C'Pro',C'Product',C'Product code'             
DD0017   DC    AL1(2),AL1(7,12),C'Product',C'Product name'                      
DD0018   DC    AL1(1),AL1(4),C'Date'                                            
DD0019   DC    AL1(1),AL1(6),C'Period'                                          
*                                                                               
DD0020   DC    AL1(1),AL1(6),C'Run on'                                          
DD0021   DC    AL1(1),AL1(2),C'At'                                              
DD0022   DC    AL1(1),AL1(6),C'Report'                                          
DD0023   DC    AL1(1),AL1(4),C'Page'                                            
DD0024   DC    AL1(1),AL1(9),C'Requestor'                                       
DD0025   DC    AL1(1),AL1(4),C'Time'                                            
DD0026   DC    AL1(1),AL1(3),C'N/d'                                             
DD0027   DC    AL1(1),AL1(2),C'  '                                              
DD0028   DC    AL1(1),AL1(2),C'  '                                              
DD0029   DC    AL1(1),AL1(2),C'  '                                              
*                                                                               
DD0030   DC    AL1(1),AL1(11),C'Combined tv'                                    
DD0031   DC    AL1(1),AL1(8),C'Coverage'                                        
DD0032   DC    AL1(1),AL1(10),C'Network tv'                                     
DD0033   DC    AL1(1),AL1(11),C'***Spill***'                                    
DD0034   DC    AL1(1),AL1(7),C'Spot tv'                                         
DD0035   DC    AL1(1),AL1(8),C'**Film**'                                        
DD0036   DC    AL1(1),AL1(18),C'***Tax excluded***'                             
DD0037   DC    AL1(1),AL1(4),C'Pnts'                                            
DD0038   DC    AL1(1),AL1(3),C'Cpp'                                             
DD0039   DC    AL1(1),AL1(4),C'Imps'                                            
*                                                                               
DD0040   DC    AL1(1),AL1(3),C'Cpm'                                             
DD0041   DC    AL1(1),AL1(4),C'Goal'                                            
DD0042   DC    AL1(3),AL1(5,9,12),C'Purch',C'Purchased'                         
         DC                   C'Purchased on'                                   
DD0043   DC    AL1(3),AL1(8,10,11),C'Achieved',C'''Achieved'''                  
         DC                    C'Achieved on'                                   
DD0044   DC    AL1(1),AL1(9),C'Affidavit'                                       
DD0045   DC    AL1(2),AL1(5,6),C'Order',C'Orderd'                               
DD0046   DC    AL1(1),AL1(7),C'Subpage'                                         
DD0047   DC    AL1(1),AL1(4),C'From'                                            
DD0048   DC    AL1(1),AL1(2),C'To'                                              
DD0049   DC    AL1(1),AL1(8),C'Estimate'                                        
*                                                                               
DD0050   DC    AL1(2),AL1(8,13),C'All ests',C'All estimates'                    
DD0051   DC    AL1(1),AL1(8),C'Est fltr'                                        
DD0052   DC    AL1(2),AL1(7,14),C'Est grp',C'Estimate group'                    
DD0053   DC    AL1(1),AL1(3),C'All'                                             
DD0054   DC    AL1(3),AL1(4,6,7),C'Mrkt',C'Market',C'Markets'                   
DD0055   DC    AL1(1),AL1(7),C'Station'                                         
DD0056   DC    AL1(1),AL1(12),C'All dayparts'                                   
DD0057   DC    AL1(1),AL1(16),C'All spot lengths'                               
DD0058   DC    AL1(1),AL1(3),C'NSI'                                             
DD0059   DC    AL1(1),AL1(3),C'ARB'                                             
*                                                                               
DD0060   DC    AL1(1),AL1(3),C'BBM'                                             
DD0061   DC    AL1(1),AL1(8),C'Affid on'                                        
DD0062   DC    AL1(1),AL1(6),C'Actual'                                          
DD0063   DC    AL1(1),AL1(4),C'Var.'                                            
DD0064   DC    AL1(1),AL1(6),C'Latest'                                          
DD0065   DC    AL1(1),AL1(4),C'Book'                                            
DD0066   DC    AL1(3),AL1(1,4,9),C'A',C'Auto',C'Automatic'                      
DD0067   DC    AL1(1),AL1(3),C'Adj'                                             
DD0068   DC    AL1(1),AL1(18),C'Based on affidavit'                             
DD0069   DC    AL1(1),AL1(16),C'Equivalence base'                               
*                                                                               
DD0070   DC    AL1(1),AL1(8),C'Sec. (+)'                                        
DD0071   DC    AL1(1),AL1(5),C'Group'                                           
DD0072   DC    AL1(1),AL1(20),C'(Goal vs. purchased)'                           
DD0073   DC    AL1(1),AL1(20),C'(Goal vs. affidavit)'                           
DD0074   DC    AL1(1),AL1(23),C'(Purchased vs. rerated)'                        
DD0075   DC    AL1(1),AL1(25),C'(Purchased vs. affidavit)'                      
DD0076   DC    AL1(1),AL1(22),C'(Lockin vs. purchased)'                         
DD0077   DC    AL1(1),AL1(22),C'(Lockin vs. affidavit)'                         
DD0078   DC    AL1(1),AL1(17),C'(Goal vs. lockin)'                              
DD0079   DC    AL1(1),AL1(32),C'(Goal vs. purchased vs. rerated)'               
*                                                                               
DD0080   DC    AL1(1),AL1(34),C'(Goal vs. purchased vs. affidavit)'             
DD0081   DC    AL1(1),AL1(29),C'(Goal vs. lockin vs. rerated)'                  
DD0082   DC    AL1(1),AL1(31),C'(Goal vs. lockin vs. affidavit)'                
DD0083   DC    AL1(1),AL1(13),C'Rating source'                                  
DD0084   DC    AL1(1),AL1(7),C'Sp@@ on'                                         
DD0085   DC    AL1(1),AL1(8),C'Ave  pct'                                        
DD0086   DC    AL1(1),AL1(5),C'Achmt'                                           
DD0087   DC    AL1(1),AL1(10),C'Daypart-ln'                                     
DD0088   DC    AL1(2),AL1(4,7),C'Dols',C'Dollars'                               
DD0089   DC    AL1(3),AL1(4,5,6),C'Spot',C'Spots',C'*Spot*'                     
*                                                                               
DD0090   DC    AL1(1),AL1(11),C'Primary dem'                                    
DD0091   DC    AL1(1),AL1(12),C'----Goal----'                                   
DD0092   DC    AL1(1),AL1(16),C'----Purchased---'                               
DD0093   DC    AL1(1),AL1(9),C'----Goal('                                       
DD0094   DC    AL1(1),AL1(9),C'Pts  pnts'                                       
DD0095   DC    AL1(1),AL1(10),C'Imps  imps'                                     
DD0096   DC    AL1(1),AL1(4),C'Pcnt'                                            
DD0097   DC    AL1(3),AL1(3,6,11),C'(A)',C'(Auto)',C'(Automatic)'               
DD0098   DC    AL1(1),AL1(2),C'  '                                              
DD0099   DC    AL1(1),AL1(18),C'*** All brands ***'                             
*                                                                               
DD0100   DC    AL1(1),AL1(6),C'Brands'                                          
DD0101   DC    AL1(1),AL1(8),C'Affd-reg'                                        
DD0102   DC    AL1(1),AL1(4),C'Spcl'                                            
DD0103   DC    AL1(1),AL1(9),C'Continued'                                       
DD0104   DC    AL1(1),AL1(4),C'Demo'                                            
DD0105   DC    AL1(1),AL1(19),C'***M o n t h l y***'                            
DD0106   DC    AL1(1),AL1(21),C'**Q u a r t e r l y**'                          
DD0107   DC    AL1(1),AL1(21),C'***Custom quarters***'                          
DD0108   DC    AL1(1),AL1(11),C'Prd(target)'                                    
DD0109   DC    AL1(1),AL1(9),C'Pcnt pcnt'                                       
*                                                                               
DD0110   DC    AL1(2),AL1(9,12),C'Demo dols',C'Demo dollars'                    
DD0111   DC    AL1(1),AL1(5),C'Cpp/m'                                           
DD0112   DC    AL1(1),AL1(22),C'* Market performance *'                         
DD0113   DC    AL1(1),AL1(16),C'--Demographics--'                               
DD0114   DC    AL1(1),AL1(9),C'Dem  demo'                                       
DD0115   DC    AL1(1),AL1(25),C'Market performance report'                      
DD0116   DC    AL1(1),AL1(20),C'Brand weekly summary'                           
DD0117   DC    AL1(1),AL1(7),C'Week of'                                         
DD0118   DC    AL1(4),AL1(3,5,7,9),C'Tot',C'Total',C'*Total*'                   
         DC                        C'**Total**'                                 
DD0119   DC    AL1(1),AL1(21),C'***Special rep=@@@***'                          
*                                                                               
DD0120   DC    AL1(1),AL1(17),C'**** Summary ****'                              
DD0121   DC    AL1(1),AL1(7),C'Pkg mst'                                         
DD0122   DC    AL1(1),AL1(7),C'Orb mst'                                         
DD0123   DC    AL1(1),AL1(7),C'Rev mst'                                         
DD0124   DC    AL1(1),AL1(4),C'*Mg*'                                            
DD0125   DC    AL1(1),AL1(4),C'Mst='                                            
DD0126   DC    AL1(1),AL1(5),C'Total'                                           
DD0127   DC    AL1(1),AL1(6),C'Tlcsts'                                          
DD0128   DC    AL1(3),AL1(6,7,10),C'Brcsts',C'Brdcsts',C'Broadcasts'            
DD0129   DC    AL1(1),AL1(3),C'Day'                                             
*                                                                               
DD0130   DC    AL1(1),AL1(11),C'Programming'                                    
DD0131   DC    AL1(1),AL1(3),C'Len'                                             
DD0132   DC    AL1(1),AL1(17),C'Rotation schedule'                              
DD0133   DC    AL1(1),AL1(25),C'Network rotation schedule'                      
DD0134   DC    AL1(1),AL1(8),C'Exchange'                                        
DD0135   DC    AL1(1),AL1(3),C'Pol'                                             
DD0136   DC    AL1(1),AL1(15),C'Address missing'                                
DD0137   DC    AL1(1),AL1(7),C'*Orbit*'                                         
DD0138   DC    AL1(1),AL1(17),C'Unallocated spots'                              
DD0139   DC    AL1(1),AL1(15),C'Pool time sheet'                                
*                                                                               
DD0140   DC    AL1(1),AL1(15),C'Pool turnaround'                                
DD0141   DC    AL1(1),AL1(15),C'Buy description'                                
DD0142   DC    AL1(1),AL1(16),C'Rotation pattern'                               
DD0143   DC    AL1(1),AL1(18),C'Demographics (@@@)'                             
DD0144   DC    AL1(1),AL1(19),C'Rated program  book'                            
DD0145   DC    AL1(1),AL1(8),C'Comments'                                        
DD0146   DC    AL1(2),AL1(3,4),C'Rep',C'Rep-'                                   
DD0147   DC    AL1(1),AL1(7),C'Channel'                                         
DD0148   DC    AL1(1),AL1(4),C'Freq'                                            
DD0149   DC    AL1(1),AL1(9),C'Affiliate'                                       
*                                                                               
DD0150   DC    AL1(1),AL1(25),C'Station rotation schedule'                      
DD0151   DC    AL1(1),AL1(19),C'***Station total***'                            
DD0152   DC    AL1(2),AL1(7,18),C'Mkt tot',C'***Market total***'                
DD0153   DC    AL1(2),AL1(7,19),C'Prd tot',C'***Product total***'               
DD0154   DC    AL1(1),AL1(9),C'No tlcsts'                                       
DD0155   DC    AL1(2),AL1(9,10),C'No brcsts',C'No brdcsts'                      
DD0156   DC    AL1(1),AL1(8),C'***Orig.'                                        
DD0157   DC    AL1(1),AL1(7),C'Mgr tot'                                         
DD0158   DC    AL1(1),AL1(4),C'Cost'                                            
DD0159   DC    AL1(1),AL1(3),C'*P*'                                             
*                                                                               
DD0160   DC    AL1(1),AL1(8),C'Wkly avg'                                        
DD0161   DC    AL1(1),AL1(9),C'Goal demo'                                       
DD0162   DC    AL1(1),AL1(6),C'Goal $'                                          
DD0163   DC    AL1(1),AL1(18),C'***Client total***'                             
DD0164   DC    AL1(1),AL1(5),C'Month'                                           
DD0165   DC    AL1(1),AL1(3),C'*M*'                                             
DD0166   DC    AL1(1),AL1(3),C'*H*'                                             
DD0167   DC    AL1(1),AL1(3),C'*U*'                                             
DD0168   DC    AL1(1),AL1(24),C'*****Product legend*****'                       
DD0169   DC    AL1(1),AL1(7),C'Unknown'                                         
*                                                                               
DD0170   DC    AL1(1),AL1(19),C'***Traffic desk ***'                            
DD0171   DC    AL1(1),AL1(14),C'***Spills from'                                 
DD0172   DC    AL1(1),AL1(7),C'Partner'                                         
DD0173   DC    AL1(1),AL1(2),C'  '                                              
DD0174   DC    AL1(1),AL1(2),C'  '                                              
DD0175   DC    AL1(1),AL1(2),C'  '                                              
DD0176   DC    AL1(1),AL1(2),C'  '                                              
DD0177   DC    AL1(1),AL1(20),C'***Cost overrides***'                           
DD0178   DC    AL1(1),AL1(20),C'*****Piggybacks*****'                           
DD0179   DC    AL1(1),AL1(2),C'Ch'                                              
*                                                                               
DD0180   DC    AL1(1),AL1(16),C'Brand timesheets'                               
DD0181   DC    AL1(1),AL1(19),C'Number of telecasts'                            
DD0182   DC    AL1(1),AL1(21),C'Demographics(imp)/cpp'                          
DD0183   DC    AL1(1),AL1(7),C'Lng/d/p'                                         
DD0184   DC    AL1(1),AL1(22),C'Salespersons signature'                         
DD0185   DC    AL1(1),AL1(24),C'Confirmation of purchase'                       
DD0186   DC    AL1(1),AL1(19),C'Daily time schedule'                            
DD0187   DC    AL1(1),AL1(26),C'Salespersons time schedule'                     
DD0188   DC    AL1(1),AL1(3),C'Lng'                                             
DD0189   DC    AL1(1),AL1(21),C'Demographics(imp=000)'                          
*                                                                               
DD0190   DC    AL1(1),AL1(19),C'Brand time schedule'                            
DD0191   DC    AL1(1),AL1(8),C'N/w time'                                        
DD0192   DC    AL1(1),AL1(2),C'Dp'                                              
DD0193   DC    AL1(1),AL1(23),C'Book    lng programming'                        
DD0194   DC    AL1(1),AL1(13),C'Cost   pt pkg'                                  
DD0195   DC    AL1(1),AL1(25),C'Demographics(imp=000)/cpp'                      
DD0196   DC    AL1(1),AL1(7),C'Network'                                         
DD0197   DC    AL1(1),AL1(6),C'Errors'                                          
DD0198   DC    AL1(1),AL1(14),C'Column numbers'                                 
DD0199   DC    AL1(1),AL1(20),C'Request card 1 image'                           
*                                                                               
DD0200   DC    AL1(1),AL1(20),C'Request card 2 image'                           
DD0201   DC    AL1(1),AL1(17),C'Field not numeric'                              
DD0202   DC    AL1(1),AL1(16),C'Not a valid date'                               
DD0203   DC    AL1(1),AL1(16),C'Estimate filters'                               
DD0204   DC    AL1(1),AL1(13),C'Product group'                                  
DD0205   DC    AL1(1),AL1(12),C'Market group'                                   
DD0206   DC    AL1(1),AL1(13),C'Process by id'                                  
DD0207   DC    AL1(1),AL1(14),C'Est- group end'                                 
DD0208   DC    AL1(1),AL1(10),C'Start date'                                     
DD0209   DC    AL1(1),AL1(8),C'End date'                                        
*                                                                               
DD0210   DC    AL1(1),AL1(20),C'Demo override active'                           
DD0211   DC    AL1(1),AL1(18),C'Affiliation filter'                             
DD0212   DC    AL1(1),AL1(19),C'Program type filter'                            
DD0213   DC    AL1(1),AL1(19),C'Daypart detail con.'                            
DD0214   DC    AL1(1),AL1(20),C'Daypart menu overide'                           
DD0215   DC    AL1(1),AL1(8),C'Option 1'                                        
DD0216   DC    AL1(1),AL1(8),C'Option 2'                                        
DD0217   DC    AL1(1),AL1(8),C'Option 3'                                        
DD0218   DC    AL1(1),AL1(8),C'Option 4'                                        
DD0219   DC    AL1(1),AL1(8),C'Option 5'                                        
*                                                                               
DD0220   DC    AL1(1),AL1(14),C'Requestor name'                                 
DD0221   DC    AL1(1),AL1(12),C'Program loop'                                   
DD0222   DC    AL1(1),AL1(20),C'Completion of report'                           
DD0223   DC    AL1(1),AL1(23),C'First request processed'                        
DD0224   DC    AL1(1),AL1(22),C'Last request processed'                         
DD0225   DC    AL1(1),AL1(24),C'Valid requests processed'                       
DD0226   DC    AL1(1),AL1(17),C'Requests in error'                              
DD0227   DC    AL1(1),AL1(24),C'Total requests processed'                       
DD0228   DC    AL1(1),AL1(18),C'Normal termination'                             
DD0229   DC    AL1(1),AL1(19),C'Operation exception'                            
*                                                                               
DD0230   DC    AL1(1),AL1(20),C'Protection exception'                           
DD0231   DC    AL1(1),AL1(20),C'Addressing exception'                           
DD0232   DC    AL1(1),AL1(14),C'Data exception'                                 
DD0233   DC    AL1(1),AL1(30),C'Summary of requests     page 1'                 
DD0234   DC    AL1(1),AL1(30),C'-------------------     ------'                 
*                                                                               
*                                                                               
DD0366   DC    AL1(1),AL1(14),C'Compete Record'                                 
DD0367   DC    AL1(1),AL1(15),C'Estimate Record'                                
DD0368   DC    AL1(1),AL1(10),C'ESB Record'                                     
DD0369   DC    AL1(1),AL1(10),C'EBD Record'                                     
*                                                                               
DD0370   DC    AL1(1),AL1(11),C'DSID Record'                                    
DD0371   DC    AL1(1),AL1(11),C'Code Record'                                    
DD0372   DC    AL1(1),AL1(10),C'PCD Record'                                     
DD0373   DC    AL1(1),AL1(10),C'SPC Record'                                     
DD0374   DC    AL1(1),AL1(10),C'NVL Record'                                     
DD0375   DC    AL1(1),AL1(12),C'ACNEQ Record'                                   
DD0376   DC    AL1(1),AL1(12),C'MGREQ Record'                                   
DD0377   DC    AL1(1),AL1(13),C'Scheme Record'                                  
DD0378   DC    AL1(1),AL1(13),C'Period Record'                                  
DD0379   DC    AL1(1),AL1(14),C'Station Record'                                 
*                                                                               
DD0380   DC    AL1(1),AL1(11),C'NSID Record'                                    
DD0381   DC    AL1(1),AL1(13),C'Detail Record'                                  
DD0382   DC    AL1(1),AL1(10),C'AOR Record'                                     
DD0383   DC    AL1(1),AL1(10),C'MTR Record'                                     
DD0384   DC    AL1(1),AL1(10),C'Buy Record'                                     
DD0385   DC    AL1(1),AL1(15),C'Stalist Record'                                 
DD0386   DC    AL1(1),AL1(15),C'Stalist Record'                                 
DD0387   DC    AL1(1),AL1(12),C'Elist Record'                                   
DD0388   DC    AL1(1),AL1(10),C'PSR Record'                                     
DD0389   DC    AL1(1),AL1(10),C'ESR Record'                                     
*                                                                               
DD0390   DC    AL1(1),AL1(14),C'MKTCORR Record'                                 
DD0391   DC    AL1(1),AL1(10),C'NDI Record'                                     
DD0392   DC    AL1(1),AL1(10),C'SPD Record'                                     
DD0393   DC    AL1(1),AL1(12),C'XRATE Record'                                   
DD0394   DC    AL1(1),AL1(13),C'Sgroup Record'                                  
DD0395   DC    AL1(1),AL1(16),C'Sgassign Record'                                
DD0396   DC    AL1(1),AL1(12),C'Sgdef Record'                                   
DD0397   DC    AL1(1),AL1(13),C'Cgroup Record'                                  
DD0398   DC    AL1(1),AL1(16),C'Cgassign Record'                                
DD0399   DC    AL1(1),AL1(12),C'Cgdef Record'                                   
*                                                                               
DD0400   DC    AL1(1),AL1(12),C'CLRST Record'                                   
DD0401   DC    AL1(1),AL1(13),C'Master Record'                                  
DD0402   DC    AL1(1),AL1(14),C'Address Record'                                 
DD0403   DC    AL1(1),AL1(13),C'Market Record'                                  
DD0404   DC    AL1(1),AL1(10),C'Rep Record'                                     
DD0405   DC    AL1(1),AL1(11),C'User Record'                                    
DD0406   DC    AL1(1),AL1(11),C'Help Record'                                    
DD0407   DC    AL1(1),AL1(12),C'Add a record'                                   
DD0408   DC    AL1(1),AL1(15),C'Change a record'                                
DD0409   DC    AL1(1),AL1(16),C'Display a record'                               
*                                                                               
DD0410   DC    AL1(1),AL1(15),C'Delete a record'                                
DD0411   DC    AL1(1),AL1(16),C'Restore a record'                               
DD0412   DC    AL1(1),AL1(12),C'List records'                                   
DD0413   DC    AL1(1),AL1(15),C'Select a record'                                
DD0414   DC    AL1(1),AL1(13),C'Copy a record'                                  
DD0415   DC    AL1(1),AL1(13),C'Rank a record'                                  
DD0416   DC    AL1(1),AL1(17),C'Transfer a record'                              
DD0417   DC    AL1(1),AL1(14),C'Pcopy a record'                                 
DD0418   DC    AL1(1),AL1(4),C'Help'                                            
*                                                                               
DDTXTX   DC    AL2(65535)                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPDDEQUS                                                       
         SPACE 1                                                                
PANACEA  EQU   24                                                               
         SPACE 1                                                                
         DC    ((((((*-BASE)/0512)+1)*0512)-PANACEA)-(*-BASE))X'00'             
BASEX    DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050SPDDENG   12/09/92'                                      
         END                                                                    
