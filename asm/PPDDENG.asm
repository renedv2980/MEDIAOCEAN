*          DATA SET PPDDENG    AT LEVEL 065 AS OF 08/09/00                      
*PHASE T00D40A                                                                  
         TITLE 'PRINT SYSTEM - DATA DICTIONARY - ENGLISH'                       
PPDDENG  CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'PPDDENG '                                                    
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
         DC    AL2(DD0027-BASE)    enter required fields or help                
         DC    AL2(DD0028-BASE)    enter all required fields                    
         DC    AL2(DD0029-BASE)    select record type and action                
*                                                                               
         DC    AL2(DD0030-BASE)    no data generated                            
         DC    AL2(DD0031-BASE)    circulation                                  
         DC    AL2(DD0032-BASE)    publication                                  
         DC    AL2(DD0033-BASE)    continued                                    
         DC    AL2(DD0034-BASE)    (continued)                                  
         DC    AL2(DD0035-BASE)    *del*                                        
         DC    AL2(DD0036-BASE)    *cha*                                        
         DC    AL2(DD0037-BASE)    *new*                                        
         DC    AL2(DD0038-BASE)    ad no.                                       
         DC    AL2(DD0039-BASE)    copy                                         
*                                                                               
         DC    AL2(DD0040-BASE)    *change summary*                             
         DC    AL2(DD0041-BASE)    revision                                     
         DC    AL2(DD0042-BASE)    *includes any proposed insertions*           
         DC    AL2(DD0043-BASE)    postings*                                    
         DC    AL2(DD0044-BASE)    *all others*                                 
         DC    AL2(DD0045-BASE)    l*                                           
         DC    AL2(DD0046-BASE)    in*                                          
         DC    AL2(DD0047-BASE)    insertion                                    
         DC    AL2(DD0048-BASE)    vendor total                                 
         DC    AL2(DD0049-BASE)    market                                       
*                                                                               
         DC    AL2(DD0050-BASE)    edition                                      
         DC    AL2(DD0051-BASE)    less c.d.                                    
         DC    AL2(DD0052-BASE)    **market totals                              
         DC    AL2(DD0053-BASE)    monthly totals                               
         DC    AL2(DD0054-BASE)    vendor                                       
         DC    AL2(DD0055-BASE)    **gross**                                    
         DC    AL2(DD0056-BASE)    **net**                                      
         DC    AL2(DD0057-BASE)    print media estimate                         
         DC    AL2(DD0058-BASE)    ** district                                  
         DC    AL2(DD0059-BASE)    ** region                                    
*                                                                               
         DC    AL2(DD0060-BASE)    ** estimate                                  
         DC    AL2(DD0061-BASE)    ** division                                  
         DC    AL2(DD0062-BASE)    flat                                         
         DC    AL2(DD0063-BASE)    open                                         
         DC    AL2(DD0064-BASE)    contract                                     
         DC    AL2(DD0065-BASE)    effective                                    
         DC    AL2(DD0066-BASE)    rate change                                  
         DC    AL2(DD0067-BASE)    pct                                          
         DC    AL2(DD0068-BASE)    non-month                                    
         DC    AL2(DD0069-BASE)    authorizations                               
*                                                                               
         DC    AL2(DD0070-BASE)    *totals*                                     
         DC    AL2(DD0071-BASE)    less authorized                              
         DC    AL2(DD0072-BASE)    qtr tots                                     
         DC    AL2(DD0073-BASE)    month                                        
         DC    AL2(DD0074-BASE)    insertion month summary **                   
         DC    AL2(DD0075-BASE)    on sale month summary **                     
         DC    AL2(DD0076-BASE)    billing month summary **                     
         DC    AL2(DD0077-BASE)    posting month summary **                     
         DC    AL2(DD0078-BASE)    payable month summary **                     
         DC    AL2(DD0079-BASE)    closing month summary **                     
*                                                                               
         DC    AL2(DD0080-BASE)    estimate totals                              
         DC    AL2(DD0081-BASE)    ** product code definitions **               
         DC    AL2(DD0082-BASE)    ** publication recap **                      
         DC    AL2(DD0083-BASE)    prd                                          
         DC    AL2(DD0084-BASE)    ** brand insertion month summary **          
         DC    AL2(DD0085-BASE)    ** brand on-sale month summary **            
         DC    AL2(DD0086-BASE)    ** brand billing month summary **            
         DC    AL2(DD0087-BASE)    ** brand payable month summary **            
         DC    AL2(DD0088-BASE)    ** brand closing month summary **            
         DC    AL2(DD0089-BASE)    space                                        
*                                                                               
         DC    AL2(DD0090-BASE)    rate                                         
         DC    AL2(DD0091-BASE)    prem/cost                                    
         DC    AL2(DD0092-BASE)    size                                         
         DC    AL2(DD0093-BASE)    displays                                     
         DC    AL2(DD0094-BASE)    show  reg  illum                             
         DC    AL2(DD0095-BASE)    description                                  
         DC    AL2(DD0096-BASE)    space description                            
         DC    AL2(DD0097-BASE)    sale                                         
         DC    AL2(DD0098-BASE)    on                                           
         DC    AL2(DD0099-BASE)    stand                                        
*                                                                               
         DC    AL2(DD0100-BASE)    bill                                         
         DC    AL2(DD0101-BASE)    ** region recap **                           
         DC    AL2(DD0102-BASE)    prd-est                                      
         DC    AL2(DD0103-BASE)    t/a                                          
         DC    AL2(DD0104-BASE)    *** billing period ***                       
         DC    AL2(DD0105-BASE)    *** payable period ***                       
         DC    AL2(DD0106-BASE)    *** on-sale dates ***                        
         DC    AL2(DD0107-BASE)    *** closing dates ***                        
         DC    AL2(DD0108-BASE)    *** clearance dates ***                      
         DC    AL2(DD0109-BASE)    vendor code                                  
*                                                                               
         DC    AL2(DD0110-BASE)    publisher code                               
         DC    AL2(DD0111-BASE)    gross less                                   
         DC    AL2(DD0112-BASE)    net cost                                     
         DC    AL2(DD0113-BASE)    cash                                         
         DC    AL2(DD0114-BASE)    discount                                     
         DC    AL2(DD0115-BASE)    gross less                                   
         DC    AL2(DD0116-BASE)    cash discount                                
         DC    AL2(DD0117-BASE)    net less                                     
         DC    AL2(DD0118-BASE)    close                                        
         DC    AL2(DD0119-BASE)    payable                                      
*                                                                               
         DC    AL2(DD0120-BASE)    lines                                        
         DC    AL2(DD0121-BASE)    (gross)                                      
         DC    AL2(DD0122-BASE)    (net)                                        
         DC    AL2(DD0123-BASE)    inches                                       
         DC    AL2(DD0124-BASE)    cost                                         
         DC    AL2(DD0125-BASE)    cost less                                    
         DC    AL2(DD0126-BASE)    change                                       
         DC    AL2(DD0127-BASE)    gross c.d.                                   
         DC    AL2(DD0128-BASE)    net c.d.                                     
         DC    AL2(DD0129-BASE)    prior                                        
*                                                                               
         DC    AL2(DD0130-BASE)    product allocation=                          
         DC    AL2(DD0131-BASE)    region/district                              
         DC    AL2(DD0132-BASE)    ** region/district recap **                  
         DC    AL2(DD0133-BASE)    ** edition totals **                         
         DC    AL2(DD0134-BASE)    ** district totals **                        
         DC    AL2(DD0135-BASE)    (s) with- auth                               
         DC    AL2(DD0136-BASE)    orizations but no buys                       
         DC    AL2(DD0137-BASE)    *region totals*                              
         DC    AL2(DD0138-BASE)    ** district recap **                         
         DC    AL2(DD0139-BASE)    totals                                       
*                                                                               
         DC    AL2(DD0140-BASE)    ** brand posting month summary **            
         DC    AL2(DD0141-BASE)    ription                                      
         DC    AL2(DD0142-BASE)    circ=,circu-                                 
         DC    AL2(DD0143-BASE)    lation                                       
         DC    AL2(DD0144-BASE)    **market totals**                            
         DC    AL2(DD0145-BASE)    *region totals*                              
         DC    AL2(DD0146-BASE)    '          '                                 
         DC    AL2(DD0147-BASE)    printpak                                     
         DC    AL2(DD0148-BASE)    subpage                                      
         DC    AL2(DD0149-BASE)    from                                         
*                                                                               
         DC    AL2(DD0150-BASE)    thru                                         
         DC    AL2(DD0151-BASE)    all                                          
         DC    AL2(DD0152-BASE)    together                                     
         DC    AL2(DD0153-BASE)    invoice                                      
         DC    AL2(DD0154-BASE)    due                                          
         DC    AL2(DD0155-BASE)    of                                           
         DC    AL2(DD0156-BASE)    (and prior)                                  
         DC    AL2(DD0157-BASE)    payee                                        
         DC    AL2(DD0158-BASE)    all ests - filtered                          
         DC    AL2(DD0159-BASE)    details of request                           
*                                                                               
         DC    AL2(DD0160-BASE)    request nnn                                  
         DC    AL2(DD0161-BASE)    request field                                
         DC    AL2(DD0162-BASE)    data                                         
         DC    AL2(DD0163-BASE)    error                                        
         DC    AL2(DD0164-BASE)    field not numeric                            
         DC    AL2(DD0165-BASE)    not a valid date                             
         DC    AL2(DD0166-BASE)    adcode                                       
         DC    AL2(DD0167-BASE)    clt                                          
         DC    AL2(DD0168-BASE)    userp code                                   
         DC    AL2(DD0169-BASE)    second estimate                              
*                                                                               
         DC    AL2(DD0170-BASE)    zone                                         
         DC    AL2(DD0171-BASE)    start date                                   
         DC    AL2(DD0172-BASE)    end date                                     
         DC    AL2(DD0173-BASE)    bill/pay date filter                         
         DC    AL2(DD0174-BASE)    sort menu                                    
         DC    AL2(DD0175-BASE)    billing mode                                 
         DC    AL2(DD0176-BASE)    class filter                                 
         DC    AL2(DD0177-BASE)    frequency filter                             
         DC    AL2(DD0178-BASE)    requestor name                               
         DC    AL2(DD0179-BASE)    comment number                               
*                                                                               
         DC    AL2(DD0180-BASE)    contract number                              
         DC    AL2(DD0181-BASE)    'as of'' control date'                       
         DC    AL2(DD0182-BASE)    current month                                
         DC    AL2(DD0183-BASE)    cut-off dateer                               
         DC    AL2(DD0184-BASE)    day                                          
         DC    AL2(DD0185-BASE)    months 12 3-byte                             
         DC    AL2(DD0186-BASE)    days    7-3byte                              
         DC    AL2(DD0187-BASE)    sale                                         
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
DD0014   DC    AL1(3),AL1(3,6,11),C'Cli',C'Client',C'** Client  '               
DD0015   DC    AL1(2),AL1(6,11),C'Client',C'Client name'                        
DD0016   DC    AL1(3),AL1(3,7,12),C'Pro',C'Product',C'** product  '             
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
DD0027   DC    AL1(1),AL1(29),C'Enter required fields or help'                  
DD0028   DC    AL1(1),AL1(25),C'Enter all required fields'                      
DD0029   DC    AL1(1),AL1(28),C'Enter record type and action'                   
*                                                                               
DD0030   DC    AL1(1),AL1(17),C'No data generated'                              
DD0031   DC    AL1(2),AL1(4,11),C'Circ',C'Circulation'                          
DD0032   DC    AL1(3),AL1(4,5,11),C'Publ',C'Publ=',C'Publication'               
DD0033   DC    AL1(2),AL1(8,9),C'Continue',C'Continued'                         
DD0034   DC    AL1(2),AL1(10,11),C'(Continue)',C'(Continued)'                   
DD0035   DC    AL1(3),AL1(3,5,6),C'Del',C'*Del*',C'Delete'                      
DD0036   DC    AL1(3),AL1(3,5,6),C'Cha',C'*Cha*',C'Change'                      
DD0037   DC    AL1(2),AL1(3,5),C'New',C'*New*'                                  
DD0038   DC    AL1(2),AL1(8,9),C'Ad no. =',C'Ad Number'                         
DD0039   DC    AL1(2),AL1(4,6),C'Copy',C'Copy ='                                
*                                                                               
DD0040   DC    AL1(1),AL1(16),C'*Change summary*'                               
DD0041   DC    AL1(2),AL1(3,8),C'Rev',C'Revision'                               
DD0042   DC    AL1(1),AL1(34),C'*Includes any proposed insertions*'             
DD0043   DC    AL1(3),AL1(7,8,9),C'Posting',C'Postings',C'Postings*'            
DD0044   DC    AL1(1),AL1(12),C'*All others*'                                   
DD0045   DC    AL1(1),AL1(2),C'L*'                                              
DD0046   DC    AL1(3),AL1(2,3,6),C'In',C'In*',C'**In**'                         
DD0047   DC    AL1(2),AL1(9,10),C'Insertion',C'Insertions'                      
DD0048   DC    AL1(2),AL1(13,17),C'Vendor totals',C'**Vendor totals**'          
DD0049   DC    AL1(3),AL1(3,5,6),C'Mkt',C'Markt',C'Market'                      
*                                                                               
DD0050   DC    AL1(2),AL1(2,7),C'Ed',C'Edition'                                 
DD0051   DC    AL1(1),AL1(9),C'Less c.d.'                                       
DD0052   DC    AL1(1),AL1(16),C'** Market totals'                               
DD0053   DC    AL1(2),AL1(8,14),C'Mo. tot.',C'Monthly totals'                   
DD0054   DC    AL1(2),AL1(4,6),C'Vend',C'Vendor'                                
DD0055   DC    AL1(2),AL1(5,9),C'Gross',C'**Gross**'                            
DD0056   DC    AL1(2),AL1(3,7),C'Net',C'**Net**'                                
DD0057   DC    AL1(1),AL1(20),C'Print media estimate'                           
DD0058   DC    AL1(3),AL1(5,8,11),C'Dist.',C'District',C'** District'           
DD0059   DC    AL1(3),AL1(5,6,9),C'Regn.',C'Region',C'** Region'                
*                                                                               
DD0060   DC    AL1(3),AL1(3,8,11),C'Est',C'Estimate',C'** Estimate'             
DD0061   DC    AL1(3),AL1(4,8,11),C'Div.',C'Division',C'** Division'            
DD0062   DC    AL1(1),AL1(4),C'Flat'                                            
DD0063   DC    AL1(1),AL1(4),C'Open'                                            
DD0064   DC    AL1(1),AL1(8),C'Contract'                                        
DD0065   DC    AL1(1),AL1(9),C'Effective'                                       
DD0066   DC    AL1(2),AL1(11,13),C'Rate Change',C'Rate Change -'                
DD0067   DC    AL1(1),AL1(3),C'Pct'                                             
DD0068   DC    AL1(2),AL1(9,11),C'Non-month',C'Non-monthly'                     
DD0069   DC    AL1(2),AL1(10,14),C'Authorized',C'Authorization'                 
*                                                                               
DD0070   DC    AL1(3),AL1(7,8,9),C'*Total*',C'*Totals*',C'**Total**'            
DD0071   DC    AL1(2),AL1(10,15),C'Less Auth.',C'Less Authorized'               
DD0072   DC    AL1(2),AL1(8,10,14),C'Qtr tots',C'Qtr totals'                    
         DC    C'Quarter totals'                                                
DD0073   DC    AL1(2),AL1(3,5),C'Mo.',C'Month'                                  
DD0074   DC    AL1(1),AL1(26),C'Insertion month summary **'                     
DD0075   DC    AL1(1),AL1(24),C'On sale month summary **'                       
DD0076   DC    AL1(1),AL1(24),C'Billing month summary **'                       
DD0077   DC    AL1(1),AL1(24),C'Posting month summary **'                       
DD0078   DC    AL1(1),AL1(24),C'Payable month summary **'                       
DD0079   DC    AL1(1),AL1(24),C'Closing Month summary **'                       
*                                                                               
DD0080   DC    AL1(2),AL1(10,16),C'Estm. tot.',C'Estimate totals'               
DD0081   DC    AL1(1),AL1(30),C'** Product code definitions **'                 
DD0082   DC    AL1(1),AL1(23),C'** Publication recap **'                        
DD0083   DC    AL1(1),AL1(3),C'prd'                                             
DD0084   DC    AL1(1),AL1(35),C'** Brand insertion month summary **'            
DD0085   DC    AL1(1),AL1(33),C'** Brand on-sale month summary **'              
DD0086   DC    AL1(1),AL1(33),C'** Brand billing month summary **'              
DD0087   DC    AL1(1),AL1(33),C'** Brand payable month summary **'              
DD0088   DC    AL1(1),AL1(33),C'** Brand closing month summary **'              
DD0089   DC    AL1(1),AL1(5),C'Space'                                           
*                                                                               
DD0090   DC    AL1(1),AL1(4),C'Rate'                                            
DD0091   DC    AL1(1),AL1(9),C'Prem/cost'                                       
DD0092   DC    AL1(2),AL1(4,5),C'Size',C'Sizes'                                 
DD0093   DC    AL1(3),AL1(7,8,10),C'Display',C'Displays',C'-Displays-'          
DD0094   DC    AL1(1),AL1(16),C'Show  reg  illum'                               
DD0095   DC    AL1(2),AL1(5,11),C'Desc.',C'Description'                         
DD0096   DC    AL1(2),AL1(9,17),C'Sp. desc.',C'Space Description'               
DD0097   DC    AL1(2),AL1(4,5),C'Sale',C'Sales'                                 
DD0098   DC    AL1(1),AL1(2),C'On'                                              
DD0099   DC    AL1(1),AL1(5),C'Stand'                                           
*                                                                               
DD0100   DC    AL1(2),AL1(4,5),C'Bill',C'Bills'                                 
DD0101   DC    AL1(1),AL1(18),C'** Region recap **'                             
DD0102   DC    AL1(1),AL1(7),C'Est-prd'                                         
DD0103   DC    AL1(1),AL1(5),C'- T/A'                                           
DD0104   DC    AL1(1),AL1(22),C'*** Billing period ***'                         
DD0105   DC    AL1(1),AL1(22),C'*** Payable period ***'                         
DD0106   DC    AL1(1),AL1(21),C'*** On-sale dates ***'                          
DD0107   DC    AL1(1),AL1(21),C'*** Closing dates ***'                          
DD0108   DC    AL1(1),AL1(23),C'*** Clearance dates ***'                        
DD0109   DC    AL1(2),AL1(6,11),C'Vendor',C'Vendor code'                        
*                                                                               
DD0110   DC    AL1(2),AL1(8,14),C'Pub code',C'Publisher code'                   
DD0111   DC    AL1(1),AL1(10),C'Gross cost'                                     
DD0112   DC    AL1(1),AL1(8),C'Net cost'                                        
DD0113   DC    AL1(1),AL1(4),C'Cash'                                            
DD0114   DC    AL1(1),AL1(8),C'Discount'                                        
DD0115   DC    AL1(1),AL1(10),C'Gross less'                                     
DD0116   DC    AL1(2),AL1(10,13),C'Cash disc.',C'Cash discount'                 
DD0117   DC    AL1(1),AL1(8),C'Net less'                                        
DD0118   DC    AL1(1),AL1(5),C'close'                                           
DD0119   DC    AL1(2),AL1(4,7),C'Pybl',C'Payable'                               
*                                                                               
DD0120   DC    AL1(2),AL1(4,5),C'Line',C'Lines'                                 
DD0121   DC    AL1(1),AL1(7),C'(Gross)'                                         
DD0122   DC    AL1(1),AL1(5),C'(Net)'                                           
DD0123   DC    AL1(2),AL1(4,6),C'Inch',C'Inches'                                
DD0124   DC    AL1(3),AL1(4,6,10),C'Cost',C'Cost =',C'      Cost'               
DD0125   DC    AL1(1),AL1(9),C'Cost less'                                       
DD0126   DC    AL1(1),AL1(18),C'** Vendor recap **'                             
DD0127   DC    AL1(2),AL1(5,10),C'Gross',C'Gross c.d.'                          
DD0128   DC    AL1(2),AL1(3,8),C'Net',C'Net c.d.'                               
DD0129   DC    AL1(1),AL1(5),C'Prior'                                           
*                                                                               
DD0130   DC    AL1(1),AL1(19),C'Product allocation='                            
DD0131   DC    AL1(2),AL1(7,15),C'Reg/dst',C'Region/District'                   
DD0132   DC    AL1(1),AL1(27),C'** Region/district recap **'                    
DD0133   DC    AL1(1),AL1(18),C'**Edition totals**'                             
DD0134   DC    AL1(1),AL1(19),C'**District totals**'                            
DD0135   DC    AL1(1),AL1(14),C'(s) with- auth'                                 
DD0136   DC    AL1(1),AL1(22),C'Orizations but no buys'                         
DD0137   DC    AL1(1),AL1(17),C'**Region totals**'                              
DD0138   DC    AL1(1),AL1(20),C'** District recap **'                           
DD0139   DC    AL1(3),AL1(4,5,6),C'Tot.',C'Total',C'Totals'                     
*                                                                               
DD0140   DC    AL1(1),AL1(33),C'** Brand posting month summary **'              
DD0141   DC    AL1(1),AL1(7),C'ription'                                         
DD0142   DC    AL1(2),AL1(5,6),C'Circ=',C'Circu-'                               
DD0143   DC    AL1(1),AL1(6),C'lation'                                          
DD0144   DC    AL1(1),AL1(17),C'**Market totals**'                              
DD0145   DC    AL1(1),AL1(15),C'*Region totals*'                                
DD0146   DC    AL1(1),AL1(10),C'          '                                     
DD0147   DC    AL1(1),AL1(8),C'Printpak'                                        
DD0148   DC    AL1(1),AL1(7),C'Subpage'                                         
DD0149   DC    AL1(1),AL1(4),C'From'                                            
*                                                                               
DD0150   DC    AL1(1),AL1(4),C'Thru'                                            
DD0151   DC    AL1(1),AL1(3),C'All'                                             
DD0152   DC    AL1(1),AL1(8),C'Together'                                        
DD0153   DC    AL1(1),AL1(7),C'Invoice'                                         
DD0154   DC    AL1(1),AL1(3),C'Due'                                             
DD0155   DC    AL1(1),AL1(2),C'Of'                                              
DD0156   DC    AL1(1),AL1(11),C'(And prior)'                                    
DD0157   DC    AL1(1),AL1(5),C'Payee'                                           
DD0158   DC    AL1(1),AL1(21),C'All ests - filtered ('                          
DD0159   DC    AL1(1),AL1(18),C'Details of request'                             
*                                                                               
DD0160   DC    AL1(1),AL1(11),C'Request nnn'                                    
DD0161   DC    AL1(1),AL1(13),C'Request field'                                  
DD0162   DC    AL1(1),AL1(4),C'Data'                                            
DD0163   DC    AL1(2),AL1(5,6),C'Error',C'Errors'                               
DD0164   DC    AL1(1),AL1(17),C'Field not numeric'                              
DD0165   DC    AL1(1),AL1(16),C'Not a valid date'                               
DD0166   DC    AL1(2),AL1(6,7),C'Adcode',C'Ad code'                             
DD0167   DC    AL1(1),AL1(3),C'Clt'                                             
DD0168   DC    AL1(1),AL1(10),C'Userp code'                                     
DD0169   DC    AL1(1),AL1(15),C'Second Estimate'                                
*                                                                               
DD0170   DC    AL1(1),AL1(4),C'Zone'                                            
DD0171   DC    AL1(1),AL1(10),C'Start Date'                                     
DD0172   DC    AL1(1),AL1(8),C'End Date'                                        
DD0173   DC    AL1(1),AL1(20),C'Bill/pay date filter'                           
DD0174   DC    AL1(1),AL1(9),C'Sort menu'                                       
DD0175   DC    AL1(1),AL1(12),C'Billing mode'                                   
DD0176   DC    AL1(1),AL1(12),C'Class filter'                                   
DD0177   DC    AL1(1),AL1(16),C'Filter frequency'                               
DD0178   DC    AL1(1),AL1(14),C'Requestor name'                                 
DD0179   DC    AL1(2),AL1(11,14),C'Comment no.',C'Comment number'               
*                                                                               
DD0180   DC    AL1(2),AL1(12,15),C'Contract no.',C'Contract number'             
DD0181   DC    AL1(1),AL1(22),C'"As of"" control date"'                         
DD0182   DC    AL1(1),AL1(13),C'Current month'                                  
DD0183   DC    AL1(1),AL1(12),C'Cut-off date'                                   
DD0184   DC    AL1(2),AL1(3,4),C'Day',C'Days'                                   
DD0185   DC    AL1(1),AL1(36),C'Janfebmaraprmayjunjulaugsepoctnovdec'           
DD0186   DC    AL1(1),AL1(21),C'Montuewedthufrisatsun'                          
DD0187   DC    AL1(1),AL1(4),C'Sale'                                            
DDTXTX   DC    AL2(65535)                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPDDEQUS                                                       
         SPACE 1                                                                
PANACEA  EQU   24                                                               
         SPACE 1                                                                
         DC    ((((((*-BASE)/0512)+1)*0512)-PANACEA)-(*-BASE))X'00'             
BASEX    DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065PPDDENG   08/09/00'                                      
         END                                                                    
