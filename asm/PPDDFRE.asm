*          DATA SET PPDDFRE    AT LEVEL 025 AS OF 08/09/00                      
*PHASE T00D44A                                                                  
         TITLE 'PRINT SYSTEM - DATA DICTIONARY - FRENCH'                        
PPDDFRE  CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'PPDDFRE '                                                    
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
         DC    AL2(DD0047-BASE)    insertions                                   
         DC    AL2(DD0048-BASE)    vendor total                                 
         DC    AL2(DD0049-BASE)    market                                       
*                                                                               
         DC    AL2(DD0050-BASE)    edition                                      
         DC    AL2(DD0051-BASE)    less c.d.                                    
         DC    AL2(DD0052-BASE)    ** market totals                             
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
         DC    AL2(DD0069-BASE)    authorization                                
*                                                                               
         DC    AL2(DD0070-BASE)    *totals*                                     
         DC    AL2(DD0071-BASE)    less authorized                              
         DC    AL2(DD0072-BASE)    qtr tots                                     
         DC    AL2(DD0073-BASE)    month                                        
         DC    AL2(DD0074-BASE)    insertion month summary**                    
         DC    AL2(DD0075-BASE)    on sale month summary **                     
         DC    AL2(DD0076-BASE)    billing mointh summary **                    
         DC    AL2(DD0077-BASE)    posting month summary **                     
         DC    AL2(DD0078-BASE)    payable month summary**                      
         DC    AL2(DD0079-BASE)    closing month summarr **                     
*                                                                               
         DC    AL2(DD0080-BASE)    estimate totals                              
         DC    AL2(DD0081-BASE)    ** product code definitions **               
         DC    AL2(DD0082-BASE)    ** publication recap **                      
         DC    AL2(DD0083-BASE)    month                                        
         DC    AL2(DD0084-BASE)    prd                                          
         DC    AL2(DD0085-BASE)    ** brand insertion month summary **          
         DC    AL2(DD0086-BASE)    ** brand on sale month summary **            
         DC    AL2(DD0087-BASE)    ** brand billing month summary **            
         DC    AL2(DD0088-BASE)    ** brand closing month summary **            
         DC    AL2(DD0089-BASE)    space                                        
*                                                                               
         DC    AL2(DD0090-BASE)    rate                                         
         DC    AL2(DD0091-BASE)    prem/cost                                    
         DC    AL2(DD0092-BASE)    size                                         
         DC    AL2(DD0093-BASE)    displays                                     
         DC    AL2(DD0094-BASE)    show reg illum                               
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
         DC    AL2(DD0104-BASE)    *** biling period ***                        
         DC    AL2(DD0105-BASE)    *** payable period ***                       
         DC    AL2(DD0106-BASE)    *** on-sale dates ***                        
         DC    AL2(DD0107-BASE)    *** closing dates ***                        
         DC    AL2(DD0108-BASE)    *** clerance dates ***                       
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
         DC    AL2(DD0135-BASE)    (s) wth- auth                                
         DC    AL2(DD0136-BASE)    orizations but no buys                       
         DC    AL2(DD0137-BASE)    *region totals*                              
         DC    AL2(DD0138-BASE)    ** district recap **                         
         DC    AL2(DD0139-BASE)    totals                                       
*                                                                               
         DC    AL2(DD0140-BASE)    ** brand posting month summary **            
         DC    AL2(DD0141-BASE)    ription                                      
         DC    AL2(DD0142-BASE)    circu-                                       
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
         DC    AL2(DD0173-BASE)    bill/pay filter                              
         DC    AL2(DD0174-BASE)    sort menu                                    
         DC    AL2(DD0175-BASE)    billing mode                                 
         DC    AL2(DD0176-BASE)    class filter                                 
         DC    AL2(DD0177-BASE)    frequency filter                             
         DC    AL2(DD0178-BASE)    requestor name                               
         DC    AL2(DD0179-BASE)    comment number                               
*                                                                               
         DC    AL2(DD0180-BASE)    contract number                              
         DC    AL2(DD0181-BASE)    'as of''control date'                        
         DC    AL2(DD0182-BASE)    current month                                
         DC    AL2(DD0183-BASE)    cut-off date                                 
         DC    AL2(DD0184-BASE)    day                                          
         DC    AL2(DD0185-BASE)    months 12 3byte                              
         DC    AL2(DD0186-BASE)    days7 3 byte                                 
         DC    AL2(DD0187-BASE)    sale                                         
*                                                                               
DDNDXX   DC    AL2(65535)                                                       
         SPACE 2                                                                
DDTXT    DS    0C                                                               
*                                                                               
DD0001   DC    AL1(2),AL1(7,15),C'Fichier',C'Type de fichier'                   
DD0002   DC    AL1(1),AL1(6),C'Action'                                          
DD0003   DC    AL1(1),AL1(14),C'Identification'                                 
DD0004   DC    AL1(1),AL1(8),C'Imprimer'                                        
DD0005   DC    AL1(1),AL1(10),C'Extraction'                                     
DD0006   DC    AL1(2),AL1(4,11),C'Dest',C'Destination'                          
DD0007   DC    AL1(1),AL1(6),C'Autres'                                          
DD0008   DC    AL1(2),AL1(5,10),C'Epure',C'Epurateurs'                          
DD0009   DC    AL1(1),AL1(7),C'Options'                                         
*                                                                               
DD0010   DC    AL1(3),AL1(3,6,16),C'Ag.',C'Agence'                              
         DC                       C'Code de l''agence'                          
DD0011   DC    AL1(2),AL1(6,15),C'Agence',C'Nom de l''agence'                   
DD0012   DC    AL1(3),AL1(3,5,13),C'Med',C'Media',C'Code du media'              
DD0013   DC    AL1(2),AL1(5,12),C'Media',C'Nom du media'                        
DD0014   DC    AL1(3),AL1(3,6,11),C'Cli',C'Client',C'** Client  '               
DD0015   DC    AL1(2),AL1(6,13),C'Client',C'Nom du client'                      
DD0016   DC    AL1(3),AL1(5,7,12),C'Prod.',C'Produit'                           
         DC                       C'** Produit  '                               
DD0017   DC    AL1(2),AL1(7,14),C'Produit',C'Nom du produit'                    
DD0018   DC    AL1(1),AL1(4),C'Date'                                            
DD0019   DC    AL1(1),AL1(7),C'Periode'                                         
*                                                                               
DD0020   DC    AL1(1),AL1(4),C'Date'                                            
DD0021   DC    AL1(1),AL1(2),C'A '                                              
DD0022   DC    AL1(1),AL1(7),C'Rapport'                                         
DD0023   DC    AL1(1),AL1(4),C'Page'                                            
DD0024   DC    AL1(1),AL1(9),C'Demanduer'                                       
DD0025   DC    AL1(1),AL1(5),C'Heure'                                           
DD0026   DC    AL1(1),AL1(3),C'N/d'                                             
DD0027   DC    AL1(1),AL1(29),C'Enter required fields or help'                  
DD0028   DC    AL1(1),AL1(25),C'Enter all required fields'                      
DD0029   DC    AL1(1),AL1(28),C'Enter record type and action'                   
*                                                                               
DD0030   DC    AL1(1),AL1(20),C'Acune donnee generee'                           
DD0031   DC    AL1(2),AL1(4,6),C'Tir.',C'Tirage'                                
DD0032   DC    AL1(3),AL1(4,5,11),C'Publ',C'Publ=',C'Publication'               
DD0033   DC    AL1(1),AL1(5),C'Suite'                                           
DD0034   DC    AL1(2),AL1(10,11),C'(Continue)',C'(Continued)'                   
DD0035   DC    AL1(3),AL1(4,6,6),C'Anul',C'*Anul*',C'Anulle'                    
DD0036   DC    AL1(3),AL1(4,6,10),C'Corr',C'*Corr*',C'Correction'               
DD0037   DC    AL1(2),AL1(7,9),C'Noureau',C'*Noreau*'                           
DD0038   DC    AL1(2),AL1(8,14),C'Num crea',C'Numero creatif'                   
DD0039   DC    AL1(2),AL1(4,6),C'Text',C'Text ='                                
*                                                                               
DD0040   DC    AL1(1),AL1(24),C'Sommaire des changements'                       
DD0041   DC    AL1(2),AL1(3,8),C'Rev',C'Revision'                               
DD0042   DC    AL1(1),AL1(38),C'Inclut toutes les insertions proposees'         
DD0043   DC    AL1(3),AL1(9,10,11),C'Affichage',C'Affichages'                   
         DC                        C'Affichages*'                               
DD0044   DC    AL1(1),AL1(15),C'Tous les autres'                                
DD0045   DC    AL1(1),AL1(2),C'L*'                                              
DD0046   DC    AL1(3),AL1(2,3,6),C'In',C'In*',C'**In**'                         
DD0047   DC    AL1(2),AL1(9,10),C'Insertion',C'Insertions'                      
DD0048   DC    AL1(2),AL1(13,18),C'Total vendeur',C'**Totaux Vendeur**'         
DD0049   DC    AL1(3),AL1(3,5,6),C'Mar',C'March',C'Marche'                      
*                                                                               
DD0050   DC    AL1(2),AL1(2,7),C'Ed',C'Edition'                                 
DD0051   DC    AL1(1),AL1(20),C'Moins esc. au compt.'                           
DD0052   DC    AL1(1),AL1(19),C'** Total par marche'                            
DD0053   DC    AL1(2),AL1(8,15),C'Tot mens',C'Totaux mensuels'                  
DD0054   DC    AL1(2),AL1(4,7),C'Vend',C'Vendeur'                               
DD0055   DC    AL1(2),AL1(4,8),C'Brut',C'**Brut**'                              
DD0056   DC    AL1(2),AL1(3,7),C'Net',C'**Net**'                                
DD0057   DC    AL1(1),AL1(20),C'Estime media imprime'                           
DD0058   DC    AL1(3),AL1(5,8,11),C'Dist.',C'District',C'** District'           
DD0059   DC    AL1(3),AL1(4,6,9),C'Reg.',C'Region',C'** Region'                 
*                                                                               
DD0060   DC    AL1(3),AL1(3,6,9),C'Est',C'Estime',C'** Estime'                  
DD0061   DC    AL1(3),AL1(4,8,11),C'Div.',C'Division',C'** Division'            
DD0062   DC    AL1(1),AL1(4),C'Fixe'                                            
DD0063   DC    AL1(1),AL1(6),C'Ouvert'                                          
DD0064   DC    AL1(1),AL1(7),C'Contrat'                                         
DD0065   DC    AL1(1),AL1(10),C'En vigueur'                                     
DD0066   DC    AL1(2),AL1(19,21),C'Changement de tarif'                         
         DC                      C'Changement de tarif -'                       
DD0067   DC    AL1(1),AL1(2),C'% '                                              
DD0068   DC    AL1(2),AL1(9,11),C'Non-mens.',C'Non-mensuel'                     
DD0069   DC    AL1(2),AL1(9,13),C'Authorize',C'Authorization'                   
*                                                                               
DD0070   DC    AL1(3),AL1(7,8,9),C'*Total*',C'*Totaux*',C'**Total**'            
DD0071   DC    AL1(2),AL1(10,19),C'Moina Aut.',C'Moins autorisations'           
DD0072   DC    AL1(2),AL1(9,20),C'Tot/trim.',C'Totaux per trimestre'            
DD0073   DC    AL1(2),AL1(3,4),C'Mo.',C'Mois'                                   
DD0074   DC    AL1(1),AL1(31),C'Sommaire mensuel des insertions'                
DD0075   DC    AL1(1),AL1(37),C'Sommaire mensuel des dates en kiosque'          
DD0076   DC    AL1(1),AL1(30),C'Sommaire facturation mensuelle'                 
DD0077   DC    AL1(1),AL1(38),C'Sommaire mensuel des dates d affichage'         
DD0078   DC    AL1(1),AL1(29),C'Sommaire des comptes payables'                  
DD0079   DC    AL1(1),AL1(32),C'Sommaire de la fermeture du mois'               
*                                                                               
DD0080   DC    AL1(2),AL1(9,15),C'Tot. est.',C'Totaux d''estime'                
DD0081   DC    AL1(1),AL1(37),C'**Definitions des codes de produits**'          
DD0082   DC    AL1(1),AL1(24),C'Sommaire par publication'                       
DD0083   DC    AL1(1),AL1(3),C'pro'                                             
DD0084   DC    AL1(1),AL1(36),C'Sommaire mensuel des ins. per marque'           
DD0085   DC    AL1(1),AL1(45),C'Sommaire mensuel des dates enniosh narmx        
                              arques'                                           
DD0086   DC    AL1(1),AL1(38),C'Sommmaire facturation mens. par marque'         
DD0087   DC    AL1(1),AL1(45),C'Sommaire mens. des cptes payables par mx        
                              arques'                                           
DD0088   DC    AL1(1),AL1(46),C'Sommaire mens. de la ferm. du mois par x        
                              marques'                                          
DD0089   DC    AL1(1),AL1(6),C'Espaca'                                          
*                                                                               
DD0090   DC    AL1(1),AL1(5),C'Tarif'                                           
DD0091   DC    AL1(1),AL1(5),C'Prime'                                           
DD0092   DC    AL1(2),AL1(6,7),C'Format',C'Formats'                             
DD0093   DC    AL1(3),AL1(7,8,10),C'Annonce',C'Annonces',C'-Announces-'         
DD0094   DC    AL1(1),AL1(45),C'Nobres d''unite   afficlies   panneaux x        
                              lumineux'                                         
DD0095   DC    AL1(2),AL1(5,11),C'Desc.',C'Description'                         
DD0096   DC    AL1(2),AL1(10,20),C'desc. esp.',C'description d''espace'         
DD0097   DC    AL1(2),AL1(5,6),C'Vente',C'Ventes'                               
DD0098   DC    AL1(1),AL1(3),C'Sur'                                             
DD0099   DC    AL1(1),AL1(5),C'Kiosk'                                           
*                                                                               
DD0100   DC    AL1(2),AL1(7,8),C'Facture',C'Factures'                           
DD0101   DC    AL1(1),AL1(20),C'**Recap par region**'                           
DD0102   DC    AL1(1),AL1(10),C'PROD. est.'                                     
DD0103   DC    AL1(1),AL1(5),C'- T/A'                                           
DD0104   DC    AL1(1),AL1(23),C'*Period de facturation*'                        
DD0105   DC    AL1(1),AL1(17),C'*** Echeanced ***'                              
DD0106   DC    AL1(1),AL1(22),C'***Dates en niosque***'                         
DD0107   DC    AL1(1),AL1(24),C'***Dates de fermeture***'                       
DD0108   DC    AL1(1),AL1(24),C'***Dates de reglement***'                       
DD0109   DC    AL1(2),AL1(7,16),C'Vendeur',C'Codes de vendeur'                  
*                                                                               
DD0110   DC    AL1(2),AL1(8,19),C'Code pub',C'Code de publication'              
DD0111   DC    AL1(1),AL1(9),C'Cout brut'                                       
DD0112   DC    AL1(1),AL1(8),C'Cout net'                                        
DD0113   DC    AL1(1),AL1(8),C'Comptant'                                        
DD0114   DC    AL1(1),AL1(8),C'Escompte'                                        
DD0115   DC    AL1(1),AL1(11),C'Brut moins'                                     
DD0116   DC    AL1(2),AL1(11,20),C'Escp au cpt',C'Escompte au comptant'         
DD0117   DC    AL1(1),AL1(9),C'Net moins'                                       
DD0118   DC    AL1(2),AL1(5,17),C'ferm ',C'date de fermeture'                   
DD0119   DC    AL1(2),AL1(4,7),C'Pybl',C'Payable'                               
*                                                                               
DD0120   DC    AL1(2),AL1(5,6),C'Ligne',C'Lignes'                               
DD0121   DC    AL1(1),AL1(6),C'(Brut)'                                          
DD0122   DC    AL1(1),AL1(5),C'(Net)'                                           
DD0123   DC    AL1(2),AL1(3,7),C'Po.',C'Pounces'                                
DD0124   DC    AL1(3),AL1(4,6,10),C'Cout',C'Cout =',C'      Cout'               
DD0125   DC    AL1(1),AL1(10),C'Cout moins'                                     
DD0126   DC    AL1(1),AL1(20),C'Sommaire par venduer'                           
DD0127   DC    AL1(2),AL1(4,21),C'Brut',C'Esc. au comptant brut'                
DD0128   DC    AL1(2),AL1(3,20),C'Net',C'Esc. au comptant net'                  
DD0129   DC    AL1(1),AL1(5),C'Avant'                                           
*                                                                               
DD0130   DC    AL1(1),AL1(23),C'Allocations par produit'                        
DD0131   DC    AL1(2),AL1(7,15),C'Reg/dst',C'Region/District'                   
DD0132   DC    AL1(1),AL1(26),C'Region/Recap. par district'                     
DD0133   DC    AL1(1),AL1(21),C'**Total par edition**'                          
DD0134   DC    AL1(1),AL1(22),C'**Total par district**'                         
DD0135   DC    AL1(1),AL1(18),C'Avec authorizations'                            
DD0136   DC    AL1(1),AL1(25),C'Authorizations sans achat'                      
DD0137   DC    AL1(1),AL1(16),C'Total par region'                               
DD0138   DC    AL1(1),AL1(18),C'Total par district'                             
DD0139   DC    AL1(3),AL1(4,5,6),C'Tot.',C'Total',C'Totaux'                     
*                                                                               
DD0140   DC    AL1(1),AL1(49),C'Sommaire mensuel des dates d''affichangx        
                              e  par marche'                                    
DD0141   DC    AL1(1),AL1(6),C'riphon'                                          
DD0142   DC    AL1(2),AL1(6,7),C'Tirage',C'Tirage='                             
DD0143   DC    AL1(1),AL1(6),C'Tirage'                                          
DD0144   DC    AL1(1),AL1(17),C'Totaux par marche'                              
DD0145   DC    AL1(1),AL1(17),C'Totaux par region'                              
DD0146   DC    AL1(1),AL1(10),C'          '                                     
DD0147   DC    AL1(1),AL1(8),C'Printpak'                                        
DD0148   DC    AL1(1),AL1(7),C'Subpage'                                         
DD0149   DC    AL1(1),AL1(2),C'De'                                              
*                                                                               
DD0150   DC    AL1(1),AL1(2),C'A '                                              
DD0151   DC    AL1(1),AL1(4),C'Tout'                                            
DD0152   DC    AL1(1),AL1(8),C'Ensemble'                                        
DD0153   DC    AL1(1),AL1(7),C'Facture'                                         
DD0154   DC    AL1(1),AL1(7),C'payable'                                         
DD0155   DC    AL1(1),AL1(2),C'De'                                              
DD0156   DC    AL1(1),AL1(12),C'Beneficiaire'                                   
DD0157   DC    AL1(1),AL1(9),C'Payable a'                                       
DD0158   DC    AL1(1),AL1(21),C'Touter camps filtress'                          
DD0159   DC    AL1(1),AL1(21),C'Details sur la demande'                         
*                                                                               
DD0160   DC    AL1(1),AL1(11),C'Demande nnn'                                    
DD0161   DC    AL1(1),AL1(18),C'Donnees de demande'                             
DD0162   DC    AL1(1),AL1(7),C'Donnee'                                          
DD0163   DC    AL1(2),AL1(6,7),C'Erreur',C'Erreurs'                             
DD0164   DC    AL1(1),AL1(18),C'Donnee non-numeric'                             
DD0165   DC    AL1(1),AL1(15),C'Date non-valide'                                
DD0166   DC    AL1(1),AL1(12),C'Code creatif'                                   
DD0167   DC    AL1(1),AL1(3),C'Clt'                                             
DD0168   DC    AL1(1),AL1(13),C'Code de userp'                                  
DD0169   DC    AL1(1),AL1(15),C'Deuxieme estime'                                
*                                                                               
DD0170   DC    AL1(1),AL1(4),C'Zone'                                            
DD0171   DC    AL1(1),AL1(14),C'Date de delout'                                 
DD0172   DC    AL1(1),AL1(10),C'Date de fin'                                    
DD0173   DC    AL1(1),AL1(45),C'Filtre par date de fracturation/comptesx        
                              payable'                                          
DD0174   DC    AL1(1),AL1(7),C'Menu de'                                         
DD0175   DC    AL1(1),AL1(19),C'Mode de facturation'                            
DD0176   DC    AL1(1),AL1(15),C'Filter de clase'                                
DD0177   DC    AL1(1),AL1(29),C'Filter par nombre de partion'                   
DD0178   DC    AL1(1),AL1(9),C'Demanduer'                                       
DD0179   DC    AL1(2),AL1(14,22),C'No commentaire',C'Numero de commentai        
                                 re'                                            
*                                                                               
DD0180   DC    AL1(2),AL1(11,17),C'No Contrato',C'Numero du contrat'            
DD0181   DC    AL1(1),AL1(25),C'Dupuis la date de control'                      
DD0182   DC    AL1(1),AL1(11),C'Moi corrant'                                    
DD0183   DC    AL1(1),AL1(17),C'Date d''annulation'                             
DD0184   DC    AL1(2),AL1(4,5),C'Jour',C'Jours'                                 
DD0185   DC    AL1(1),AL1(36),C'JANFEVMARAVRMAIJUNJULAOUSEPOCTNOVDEC'           
DD0186   DC    AL1(1),AL1(21),C'LUNMARMERJEUVENSAMDIM'                          
DD0187   DC    AL1(1),AL1(4),C'Vent'                                            
DDTXTX   DC    AL2(65535)                                                       
         EJECT                                                                  
       ++INCLUDE PPDDEQUS                                                       
         SPACE 1                                                                
PANACEA  EQU   24                                                               
         SPACE 1                                                                
         DC    ((((((*-BASE)/0512)+1)*0512)-PANACEA)-(*-BASE))X'00'             
BASEX    DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025PPDDFRE   08/09/00'                                      
         END                                                                    
