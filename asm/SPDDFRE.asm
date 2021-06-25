*          DATA SET SPDDFRE    AT LEVEL 023 AS OF 03/12/90                      
*PHASE T00D24                                                                   
         TITLE 'SPOT SYSTEM - DATA DICTIONARY - FRENCH'                         
SPDDFRE  CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'SPDDFRE '                                                    
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
         DC    AL2(DD0042-BASE)    purch,purchased,purchased on                 
         DC    AL2(DD0043-BASE)    achieved                                     
         DC    AL2(DD0044-BASE)    affidavit                                    
         DC    AL2(DD0045-BASE)    order,orderd                                 
         DC    AL2(DD0046-BASE)    subpage                                      
         DC    AL2(DD0047-BASE)    from                                         
         DC    AL2(DD0048-BASE)    to                                           
         DC    AL2(DD0049-BASE)    estimate                                     
*                                                                               
         DC    AL2(DD0050-BASE)    all estiamtes                                
         DC    AL2(DD0051-BASE)    est fltr                                     
         DC    AL2(DD0052-BASE)    estimate group                               
         DC    AL2(DD0053-BASE)    all                                          
         DC    AL2(DD0054-BASE)    mrkt,market,markets                          
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
         DC    AL2(DD0088-BASE)    dols,dollars                                 
         DC    AL2(DD0089-BASE)    spots                                        
*                                                                               
         DC    AL2(DD0090-BASE)    primary dem                                  
         DC    AL2(DD0091-BASE)    ----goal----                                 
         DC    AL2(DD0092-BASE)    ----purchased---                             
         DC    AL2(DD0093-BASE)    ----goal(                                    
         DC    AL2(DD0094-BASE)    pts  pnts                                    
         DC    AL2(DD0095-BASE)    imps  imps                                   
         DC    AL2(DD0096-BASE)    pcnt                                         
         DC    AL2(DD0097-BASE)    (a),(auto),a(automatic)                      
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
         DC    AL2(DD0118-BASE)    **total**                                    
         DC    AL2(DD0119-BASE)    ***special rep=@@@***                        
*                                                                               
         DC    AL2(DD0120-BASE)    **** summary ****                            
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
         DC    AL2(DD0151-BASE)    ***station total***                          
         DC    AL2(DD0152-BASE)    mkt tot,***market total***                   
         DC    AL2(DD0153-BASE)    prd tot,***product total***                  
         DC    AL2(DD0154-BASE)    no tlcsts                                    
         DC    AL2(DD0155-BASE)    no brcsts,no brdcsts                         
         DC    AL2(DD0156-BASE)    ***org.                                      
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
         DC    AL2(DD0192-BASE)    pj                                           
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
         DC    AL2(DD0228-BASE)    normal termiantion                           
         DC    AL2(DD0229-BASE)    operation exception                          
*                                                                               
         DC    AL2(DD0230-BASE)    protection exception                         
         DC    AL2(DD0231-BASE)    addressing exception                         
         DC    AL2(DD0232-BASE)    data exception                               
         DC    AL2(DD0233-BASE)    summary of requests     page 1               
         DC    AL2(DD0234-BASE)    -------------------     ------               
*                                                                               
DDNDXX   DC    AL2(65535)                                                       
         SPACE 2                                                                
DDTXT    DS    0C                                                               
*                                                                               
DD0001   DC    AL1(2),AL1(7,14),C'Fichier',C'Type de fichier'                   
DD0002   DC    AL1(1),AL1(6),C'Action'                                          
DD0003   DC    AL1(1),AL1(14),C'Identification'                                 
DD0004   DC    AL1(1),AL1(8),C'Imprimer'                                        
DD0005   DC    AL1(1),AL1(10),C'Extraction'                                     
DD0006   DC    AL1(2),AL1(5,11),C'Dest.',C'Destination'                         
DD0007   DC    AL1(1),AL1(6),C'Autres'                                          
DD0008   DC    AL1(2),AL1(10,5),C'Epurateurs',C'Epure'                          
DD0009   DC    AL1(1),AL1(7),C'Options'                                         
*                                                                               
DD0010   DC    AL1(3),AL1(3,6,16),C'Ag.',C'Agence'                              
         DC                       C'Code de l''agence'                          
DD0011   DC    AL1(2),AL1(6,15),C'Agence',C'Nom de l''agence'                   
DD0012   DC    AL1(3),AL1(3,5,13),C'Med',C'Media',C'Code du media'              
DD0013   DC    AL1(2),AL1(5,12),C'Media',C'Nom du media'                        
DD0014   DC    AL1(3),AL1(4,6,14),C'Cli.',C'Client',C'Code du client'           
DD0015   DC    AL1(2),AL1(6,13),C'Client',C'Nom du client'                      
DD0016   DC    AL1(3),AL1(5,7,15),C'Prod.',C'Produit'                           
         DC                       C'Code du produit'                            
DD0017   DC    AL1(2),AL1(7,14),C'Produit',C'Nom du produit'                    
DD0018   DC    AL1(1),AL1(4),C'Date'                                            
DD0019   DC    AL1(1),AL1(7),C'Periode'                                         
*                                                                               
DD0020   DC    AL1(1),AL1(4),C'Date'                                            
DD0021   DC    AL1(1),AL1(2),C'A '                                              
DD0022   DC    AL1(1),AL1(7),C'Rapport'                                         
DD0023   DC    AL1(1),AL1(4),C'Page'                                            
DD0024   DC    AL1(1),AL1(9),C'Demandeur'                                       
DD0025   DC    AL1(1),AL1(5),C'Heure'                                           
DD0026   DC    AL1(1),AL1(3),C'N/d'                                             
DD0027   DC    AL1(1),AL1(2),C'  '                                              
DD0028   DC    AL1(1),AL1(2),C'  '                                              
DD0029   DC    AL1(1),AL1(2),C'  '                                              
*                                                                               
DD0030   DC    AL1(1),AL1(16),C'Achat TV combine'                               
DD0031   DC    AL1(1),AL1(10),C'Couverture'                                     
DD0032   DC    AL1(1),AL1(9),C'Reseau TV'                                       
DD0033   DC    AL1(1),AL1(11),C'Debordement'                                    
DD0034   DC    AL1(1),AL1(11),C'Occasion TV'                                    
DD0035   DC    AL1(1),AL1(8),C'**Film**'                                        
DD0036   DC    AL1(1),AL1(18),C'***Taxe exclue ***'                             
DD0037   DC    AL1(1),AL1(5),C'Cotes'                                           
DD0038   DC    AL1(1),AL1(3),C'Cpp'                                             
DD0039   DC    AL1(1),AL1(4),C'Imps'                                            
*                                                                               
DD0040   DC    AL1(1),AL1(3),C'Cpm'                                             
DD0041   DC    AL1(1),AL1(8),C'Objectif'                                        
DD0042   DC    AL1(3),AL1(5,8,14),C'Achat',C'''Achete'''                        
         DC                       C'Achat base sur'                             
DD0043   DC    AL1(3),AL1(7,9,11),C'Realise',C'''Realise'''                     
         DC                       C'Realise sur'                                
DD0044   DC    AL1(1),AL1(9),C'Affidavit'                                       
DD0045   DC    AL1(2),AL1(8,8),C'Commande',C'Commande'                          
DD0046   DC    AL1(1),AL1(17),C'Sommaire detaille'                              
DD0047   DC    AL1(1),AL1(2),C'Du'                                              
DD0048   DC    AL1(1),AL1(2),C'Au'                                              
DD0049   DC    AL1(1),AL1(6),C'Estime'                                          
*                                                                               
DD0050   DC    AL1(2),AL1(8,13),C'Est. gr.',C'Estime groupe'                    
DD0051   DC    AL1(1),AL1(12),C'Estime epure'                                   
DD0052   DC    AL1(2),AL1(8,14),C'Grp camp',C'Group campagne'                   
DD0053   DC    AL1(1),AL1(4),C'Tous'                                            
DD0054   DC    AL1(3),AL1(4,6,7),C'Mar.',C'Marche',C'Marches'                   
DD0055   DC    AL1(1),AL1(7),C'Station'                                         
DD0056   DC    AL1(1),AL1(14),C'Blocs horaires'                                 
DD0057   DC    AL1(1),AL1(5),C'Duree'                                           
DD0058   DC    AL1(1),AL1(3),C'Nsi'                                             
DD0059   DC    AL1(1),AL1(3),C'Arb'                                             
*                                                                               
DD0060   DC    AL1(1),AL1(3),C'Bbm'                                             
DD0061   DC    AL1(1),AL1(22),C'Approuve sur affidavit'                         
DD0062   DC    AL1(1),AL1(6),C'Actuel'                                          
DD0063   DC    AL1(1),AL1(4),C'Var.'                                            
DD0064   DC    AL1(1),AL1(7),C'Dernier'                                         
DD0065   DC    AL1(1),AL1(7),C'Rapport'                                         
DD0066   DC    AL1(3),AL1(1,4,11),C'A',C'Auto',C'Automatique'                   
DD0067   DC    AL1(2),AL1(4,6),C'Ajus',C'Ajuste'                                
DD0068   DC    AL1(1),AL1(17),C'D''apres affidavit'                             
DD0069   DC    AL1(1),AL1(18),C'Base d''equivalence'                            
*                                                                               
DD0070   DC    AL1(1),AL1(8),C'Sec. (+)'                                        
DD0071   DC    AL1(1),AL1(6),C'Groupe'                                          
DD0072   DC    AL1(1),AL1(19),C'(Objectif vs achat)'                            
DD0073   DC    AL1(1),AL1(23),C'(Objectif vs affidavit)'                        
DD0074   DC    AL1(1),AL1(23),C'(Achat vs post-analyse)'                        
DD0075   DC    AL1(1),AL1(20),C'(Achat vs affidavit)'                           
DD0076   DC    AL1(1),AL1(19),C'("Lockin" vs achat)'                            
DD0077   DC    AL1(1),AL1(23),C'("Lockin" vs affidavit)'                        
DD0078   DC    AL1(1),AL1(22),C'(Objectif vs "lockin")'                         
DD0079   DC    AL1(1),AL1(35),C'(Objectif vs achat vs post-analyse)'            
*                                                                               
DD0080   DC    AL1(1),AL1(32),C'(Objectif vs achat vs affidavit)'               
DD0081   DC    AL1(1),AL1(39)                                                   
         DC           C'(Objectif vs "lockin" vs nouveau tarif)'                
DD0082   DC    AL1(1),AL1(35),C'(Objectif vs "lockin" vs affidavit)'            
DD0083   DC    AL1(1),AL1(17),C'Source du sondage'                              
DD0084   DC    AL1(1),AL1(7),C'Sp@@ le'                                         
DD0085   DC    AL1(1),AL1(8),C'Moyen  %'                                        
DD0086   DC    AL1(1),AL1(4),C'Reel'                                            
DD0087   DC    AL1(1),AL1(14),C'Blocs horaires'                                 
DD0088   DC    AL1(2),AL1(4,7),C'Dols',C'Dollars'                               
DD0089   DC    AL1(1),AL1(4,6),C'Occ.',C'*Occ.*'                                
*                                                                               
DD0090   DC    AL1(1),AL1(13),C'Demo primaire'                                  
DD0091   DC    AL1(1),AL1(12),C'--Objectif--'                                   
DD0092   DC    AL1(1),AL1(16),C'-----Achete-----'                               
DD0093   DC    AL1(2),AL1(9,11),C'--Object(',C'--Objectif('                     
DD0094   DC    AL1(1),AL1(9),C'Peb peb''s'                                      
DD0095   DC    AL1(1),AL1(10),C'Imp. imps.'                                     
DD0096   DC    AL1(1),AL1(2),C'% '                                              
DD0097   DC    AL1(3),AL1(3,6,13),C'(A)',C'(Auto)',C'(Automatique)'             
DD0098   DC    AL1(1),AL1(2),C'  '                                              
DD0099   DC    AL1(1),AL1(18),C'Toutes les marques'                             
*                                                                               
DD0100   DC    AL1(1),AL1(7),C'Marques'                                         
DD0101   DC    AL1(1),AL1(14),C'Affidavit-reg.'                                 
DD0102   DC    AL1(1),AL1(5),C'Spec.'                                           
DD0103   DC    AL1(1),AL1(5),C'Suite'                                           
DD0104   DC    AL1(1),AL1(4),C'Demo'                                            
DD0105   DC    AL1(1),AL1(18),C'**** Mensuel *****'                             
DD0106   DC    AL1(1),AL1(21),C'**** Trimestrial ****'                          
DD0107   DC    AL1(1),AL1(21),C'*** Trim. speciaux **'                          
DD0108   DC    AL1(1),AL1(10),C'Prd(cible)'                                     
DD0109   DC    AL1(1),AL1(9),C'  %    % '                                       
*                                                                               
DD0110   DC    AL1(2),AL1(9,12),C'Demo dols',C'Demo dollars'                    
DD0111   DC    AL1(1),AL1(5),C'Cpp/m'                                           
DD0112   DC    AL1(1),AL1(18),C'Performance marche'                             
DD0113   DC    AL1(1),AL1(16),C'-Demographiques-'                               
DD0114   DC    AL1(1),AL1(9),C'Dem  demo'                                       
DD0115   DC    AL1(1),AL1(26),C'Rapport performance marche'                     
DD0116   DC    AL1(1),AL1(32),C'Resume hebdomadaire des produits'               
DD0117   DC    AL1(1),AL1(10),C'Semaine du'                                     
DD0118   DC    AL1(4),AL1(3,5,7,9),C'Tot',C'Total',C'*Total*'                   
         DC                        C'**Total**'                                 
DD0119   DC    AL1(1),AL1(24),C'Representant special=@@@'                       
*                                                                               
DD0120   DC    AL1(1),AL1(17),C'****Sommaire ****'                              
DD0121   DC    AL1(2),AL1(4,9),C'Plan',C'Plan type'                             
DD0122   DC    AL1(2),AL1(8,13),C'Pl. rot.',C'Plan rotation'                    
DD0123   DC    AL1(2),AL1(8,16),C'Rev. Pl.',C'Revision du plan'                 
DD0124   DC    AL1(1),AL1(7),C'Reprise'                                         
DD0125   DC    AL1(1),AL1(7),C'Maitre='                                         
DD0126   DC    AL1(1),AL1(5),C'Total'                                           
DD0127   DC    AL1(1),AL1(13),C'Telediffusion'                                  
DD0128   DC    AL1(1),AL1(9),C'Diffusion'                                       
DD0129   DC    AL1(1),AL1(4),C'Jour'                                            
*                                                                               
DD0130   DC    AL1(1),AL1(13),C'Programmation'                                  
DD0131   DC    AL1(2),AL1(4,5),C'Dur.',C'Duree'                                 
DD0132   DC    AL1(1),AL1(16),C'Horaire rotation'                               
DD0133   DC    AL1(1),AL1(23),C'Horaire rotation reseau'                        
DD0134   DC    AL1(1),AL1(8),C'Exchange'                                        
DD0135   DC    AL1(1),AL1(3),C'Pol'                                             
DD0136   DC    AL1(1),AL1(17),C'Addresse inconnue'                              
DD0137   DC    AL1(1),AL1(7),C'*Orbit*'                                         
DD0138   DC    AL1(1),AL1(21),C'Produit non-determine'                          
DD0139   DC    AL1(1),AL1(10),C'Calendrier'                                     
*                                                                               
DD0140   DC    AL1(1),AL1(15),C'Pool turnaround'                                
DD0141   DC    AL1(1),AL1(18),C'Description d''achat'                           
DD0142   DC    AL1(1),AL1(16),C'Rotation pattern'                               
DD0143   DC    AL1(1),AL1(18),C'Demographique(@@@)'                             
DD0144   DC    AL1(1),AL1(19),C'Rated program  book'                            
DD0145   DC    AL1(1),AL1(12),C'Commentaires'                                   
DD0146   DC    AL1(3),AL1(3,4,5),C'Agn',C'Agn-',C'Agent'                        
DD0147   DC    AL1(1),AL1(6),C'Chaine'                                          
DD0148   DC    AL1(1),AL1(4),C'Freq'                                            
DD0149   DC    AL1(1),AL1(7),C'Affilie'                                         
*                                                                               
DD0150   DC    AL1(1),AL1(22),C'Calendrier par station'                         
DD0151   DC    AL1(1),AL1(17),C'Total par station'                              
DD0152   DC    AL1(2),AL1(7,19),C'Tot mkt',C'*Total par marchee*'               
DD0153   DC    AL1(2),AL1(7,19),C'Tot prd',C'*Total par produit*'               
DD0154   DC    AL1(1),AL1(14),C'Par d''annonces'                                
DD0155   DC    AL1(2),AL1(9,10),C'No brcsts',C'No brdcsts'                      
DD0156   DC    AL1(1),AL1(8),C'***Orig.'                                        
DD0157   DC    AL1(2),AL1(7,19),C'Tot mgr',C'Total group marchee'               
DD0158   DC    AL1(1),AL1(4),C'Cout'                                            
DD0159   DC    AL1(1),AL1(4),C'*P*'                                             
*                                                                               
DD0160   DC    AL1(2),AL1(8,19),C'Moyn sem',C'Moyenne par semaine'              
DD0161   DC    AL1(1),AL1(12),C'Demo budgete'                                   
DD0162   DC    AL1(1),AL1(9),C'$ budgete'                                       
DD0163   DC    AL1(1),AL1(17),C'*Total par Client*'                             
DD0164   DC    AL1(1),AL1(4),C'Mois'                                            
DD0165   DC    AL1(1),AL1(3),C'*M*'                                             
DD0166   DC    AL1(1),AL1(3),C'*H*'                                             
DD0167   DC    AL1(1),AL1(3),C'*U*'                                             
DD0168   DC    AL1(1),AL1(24),C'**Legende des produits**'                       
DD0169   DC    AL1(1),AL1(7),C'Inconnu'                                         
*                                                                               
DD0170   DC    AL1(1),AL1(21),C'*Bureau de tallicque*'                          
DD0171   DC    AL1(1),AL1(12),C'***Spills de'                                   
DD0172   DC    AL1(1),AL1(10),C'Partenaire'                                     
DD0173   DC    AL1(1),AL1(2),C'  '                                              
DD0174   DC    AL1(1),AL1(2),C'  '                                              
DD0175   DC    AL1(1),AL1(2),C'  '                                              
DD0176   DC    AL1(1),AL1(2),C'  '                                              
DD0177   DC    AL1(1),AL1(20),C'***Cost overrides***'                           
DD0178   DC    AL1(1),AL1(20),C'*****Piggybacks*****'                           
DD0179   DC    AL1(1),AL1(2),C'Ch'                                              
*                                                                               
DD0180   DC    AL1(1),AL1(19),C'Horaire par produit'                            
DD0181   DC    AL1(1),AL1(17),C'Nombre d''annonces'                             
DD0182   DC    AL1(1),AL1(21),C'Demographique,imp/cpp'                          
DD0183   DC    AL1(1),AL1(3),C'Lng'                                             
DD0184   DC    AL1(1),AL1(20),C'Signature du vendeur'                           
DD0185   DC    AL1(1),AL1(20),C'Confirmation d''achat'                          
DD0186   DC    AL1(1),AL1(19),C'Calendrier par jour'                            
DD0187   DC    AL1(1),AL1(3),C'Calendrier pour vendeur'                         
DD0188   DC    AL1(1),AL1(3),C'Lng'                                             
DD0189   DC    AL1(1),AL1(19),C'Demographie, imp/000'                           
*                                                                               
DD0190   DC    AL1(1),AL1(19),C'Horaire par produit'                            
DD0191   DC    AL1(1),AL1(6),C'#/sem.'                                          
DD0192   DC    AL1(1),AL1(2),C'Pj'                                              
DD0193   DC    AL1(1),AL1(21),C'Rapport lng programme'                          
DD0194   DC    AL1(1),AL1(13),C'Cout   pt pkg'                                  
DD0195   DC    AL1(1),AL1(26),C'Demographique(imp=000)/cpp'                     
DD0196   DC    AL1(1),AL1(11),C'Tv affiliee'                                    
DD0197   DC    AL1(1),AL1(7),C'Erreurs'                                         
DD0198   DC    AL1(1),AL1(18),C'Numeros de colonne'                             
DD0199   DC    AL1(1),AL1(20),C'Request card 1 image'                           
*                                                                               
DD0200   DC    AL1(1),AL1(20),C'Request card 2 image'                           
DD0201   DC    AL1(1),AL1(20),C'Donnee non numerique'                           
DD0202   DC    AL1(1),AL1(15),C'Date non valide'                                
DD0203   DC    AL1(1),AL1(14),C'Estimes epures'                                 
DD0204   DC    AL1(1),AL1(17),C'Group de produits'                              
DD0205   DC    AL1(1),AL1(16),C'Group de marches'                               
DD0206   DC    AL1(1),AL1(13),C'Process by id'                                  
DD0207   DC    AL1(1),AL1(21),C'Fin du group d''estime'                         
DD0208   DC    AL1(1),AL1(11),C'Commence le'                                    
DD0209   DC    AL1(1),AL1(7),C'Fini le'                                         
*                                                                               
DD0210   DC    AL1(1),AL1(16),C'Peb/aud estimame'                               
DD0211   DC    AL1(1),AL1(24),C'Indicatif d''affiliation'                       
DD0212   DC    AL1(1),AL1(30),C'Indicatif de type de programme'                 
DD0213   DC    AL1(1),AL1(25),C'Detail des blocs horaires'                      
DD0214   DC    AL1(1),AL1(27),C'Blocs horaires sur commande'                    
DD0215   DC    AL1(1),AL1(8),C'Option 1'                                        
DD0216   DC    AL1(1),AL1(8),C'Option 2'                                        
DD0217   DC    AL1(1),AL1(8),C'Option 3'                                        
DD0218   DC    AL1(1),AL1(8),C'Option 4'                                        
DD0219   DC    AL1(1),AL1(8),C'Option 5'                                        
*                                                                               
DD0220   DC    AL1(1),AL1(16),C'Nom du demandeur'                               
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
**PAN#1  DC    CL21'023SPDDFRE   03/12/90'                                      
         END                                                                    
