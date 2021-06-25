*          DATA SET SRDDGER    AT LEVEL 002 AS OF 08/22/00                      
*PHASE T00D13A                                                                  
         TITLE 'SVC SYSTEM - DATA DICTIONARY - GERMAN'                          
SRDDGER  CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'SRDDGER '                                                    
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
         DC    AL2(DD0193-BASE)    line id        (transmission)                
         DC    AL2(DD0194-BASE)    purged                                       
         DC    AL2(DD0195-BASE)    live                                         
         DC    AL2(DD0196-BASE)    dead                                         
         DC    AL2(DD0197-BASE)    wide                                         
         DC    AL2(DD0198-BASE)    sort                                         
         DC    AL2(DD0199-BASE)    cdate          (create date)                 
*                                                                               
         DC    AL2(DD0200-BASE)    pdate          (printed date)                
         DC    AL2(DD0201-BASE)    sdate          (sent date)                   
         DC    AL2(DD0202-BASE)    rdate          (retain date)                 
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
DD0001   DC    AL1(1,05),C'Seite'                                               
DD0002   DC    AL1(1,06),C'Seiten'                                              
DD0003   DC    AL1(2,4,7),C'Zchn',C'Zeichen'                                    
DD0004   DC    AL1(1,08),C'gefunden'                                            
DD0005   DC    AL1(1,06),C'Puffer'                                              
DD0006   DC    AL1(1,05),C'Suche'                                               
DD0007   DC    AL1(2,05,06),C'rchts',C'rechts'                                  
DD0008   DC    AL1(2,03,05),C'lks',C'links'                                     
DD0009   DC    AL1(2,06,08),C'Weiter',C'Bild vor'                               
*                                                                               
DD0010   DC    AL1(2,06,09),C'Zur}ck',C'Bild zrck'                              
DD0011   DC    AL1(1,08),C'Spool-ID'                                            
DD0012   DC    AL1(1,05),C'Zeile'                                               
DD0013   DC    AL1(2,2,6),C'Sp',C'Spalte'                                       
DD0014   DC    AL1(1,06),C'Option'                                              
DD0015   DC    AL1(1,06),C'Rollen'                                              
DD0016   DC    AL1(2,08,09),C'Vorseite',C'Seite vor'                            
DD0017   DC    AL1(2,10,13),C'Seite zrck',C'N{chste Seite'                      
DD0018   DC    AL1(1,09),C'Vorgemrkt'                                           
DD0019   DC    AL1(1,06),C'submit'                                              
*                                                                               
DD0020   DC    AL1(1,04),C'Alle'                                                
DD0021   DC    AL1(1,06),C'ndern'                                              
DD0022   DC    AL1(1,11),C'Datum=heute'                                         
DD0023   DC    AL1(1,05),C'lu id'                                               
DD0024   DC    AL1(1,09),C'Angezeigt'                                           
DD0025   DC    AL1(3,3,4,6),C'Hlt',C'Halt',C'Halten'                            
DD0026   DC    AL1(2,04,05),C'Aktv',C'Aktiv'                                    
DD0027   DC    AL1(3,02,04,08),C'Sd',C'Gsdt',C'Gesendet'                        
DD0028   DC    AL1(3,04,06,08),C'Gedr',C'Druck ',C'Gedruckt'                    
DD0029   DC    AL1(2,04,06),C'node',C'Knoten'                                   
*                                                                               
DD0030   DC    AL1(2,4,5),C'Strt',C'Start'                                      
DD0031   DC    AL1(3,3,4,8),C'Anz',C'Anzg',C'Anzeigen'                          
DD0032   DC    AL1(1,07),C'Drucker'                                             
DD0033   DC    AL1(1,07),C'Shuttle'                                             
DD0034   DC    AL1(1,04),C'auto'                                                
DD0035   DC    AL1(1,07),C'inaktiv'                                             
DD0036   DC    AL1(1,05),C'Daten'                                               
DD0037   DC    AL1(1,08),C'Gestoppt'                                            
DD0038   DC    AL1(1,12),C'wegen Fehler'                                        
DD0039   DC    AL1(2,4,6),C'Drck',C'Druckt'                                     
*                                                                               
DD0040   DC    AL1(1,06),C'Sendet'                                              
DD0041   DC    AL1(1,05),C'Seite'                                               
DD0042   DC    AL1(1,09),C'gestartet'                                           
DD0043   DC    AL1(1,34),C'*** Report gelºscht/verfallen ****'                  
DD0044   DC    AL1(1,07),C'Sprache'                                             
DD0045   DC    AL1(1,04),C'Land'                                                
DD0046   DC    AL1(1,16),C'Status angezeigt'                                    
DD0047   DC    AL1(1,09),C'freigeben'                                           
DD0048   DC    AL1(1,6),C'zur}ck'                                               
DD0049   DC    AL1(1,22),C'Zum Stoppen vorgesehen'                              
*                                                                               
DD0050   DC    AL1(1,23),C'Status und PQ angezeigt'                             
DD0051   DC    AL1(1,12),C'Neuer Status'                                        
DD0052   DC    AL1(2,03,06),C'Zln',C'Zeilen'                                    
DD0053   DC    AL1(1,05),C'Druck'                                               
DD0054   DC    AL1(1,04),C'Zeit'                                                
DD0055   DC    AL1(1,09),C'Reporttyp'                                           
DD0056   DC    AL1(2,10,13),C'Rpt-Filter',C'Report-Filter'                      
DD0057   DC    AL1(1,09),C'permanent'                                           
DD0058   DC    AL1(1,04),C'null'                                                
DD0059   DC    AL1(2,7,3),C'Stunden',C'Std'                                     
*                                                                               
DD0060   DC    AL1(1,16),C'Report angezeigt'                                    
DD0061   DC    AL1(1,15),C'Report gelºscht'                                     
DD0062   DC    AL1(1,11),C'Komprimiert'                                         
DD0063   DC    AL1(1,05),C'Mitte'                                               
DD0064   DC    AL1(1,07),C'Zentrum'                                             
DD0065   DC    AL1(2,6,11),C'Ben-ID',C'Benutzer-ID'                             
DD0066   DC    AL1(1,06),C'Anzahl'                                              
DD0067   DC    AL1(1,05),C'Erste'                                               
DD0068   DC    AL1(2,7,12),C'Ben-IDs',C'Benutzer-IDs'                           
DD0069   DC    AL1(1,09),C'in Arbeit'                                           
*                                                                               
DD0070   DC    AL1(2,7,8),C'N{chste',C'N{chster'                                
DD0071   DC    AL1(1,21),C'alle Reports entfernt'                               
DD0072   DC    AL1(1,07),C'Sichern'                                             
DD0073   DC    AL1(2,04,06),C'Gsmt',C'Gesamt'                                   
DD0074   DC    AL1(2,04,08),C'Flg#',C'Folge-Nr'                                 
DD0075   DC    AL1(1,07),C'Geplant'                                             
DD0076   DC    AL1(1,07),C'Ausgabe'                                             
DD0077   DC    AL1(1,11),C'Seite,Zeile'                                         
DD0078   DC    AL1(1,03),C'von'                                                 
DD0079   DC    AL1(1,03),C'bis'                                                 
*                                                                               
DD0080   DC    AL1(1,12),C'Reportanzahl'                                        
DD0081   DC    AL1(2,7,09),C'Rep.-Id',C'Report-Id'                              
DD0082   DC    AL1(1,11),C'Lºschung am'                                         
DD0083   DC    AL1(1,12),C'Reportklasse'                                        
DD0084   DC    AL1(1,11),C'Lºschung um'                                         
DD0085   DC    AL1(1,10),C'Reportname'                                          
DD0086   DC    AL1(1,04),C'hoch'                                                
DD0087   DC    AL1(1,06),C'Status'                                              
DD0088   DC    AL1(1,11),C'Erstellt am'                                         
DD0089   DC    AL1(1,06),C'Seiten'                                              
*                                                                               
DD0090   DC    AL1(1,11),C'Erstellt um'                                         
DD0091   DC    AL1(1,06),C'Zeilen'                                              
DD0092   DC    AL1(1,19),C'Speicherzeit/aktive'                                 
DD0093   DC    AL1(1,08),C'Druckort'                                            
DD0094   DC    AL1(1,11),C'Gedruckt am'                                         
DD0095   DC    AL1(1,09),C'Ausdrucke'                                           
DD0096   DC    AL1(1,11),C'Gedruckt um'                                         
DD0097   DC    AL1(1,07),C'Drucker'                                             
DD0098   DC    AL1(1,22),C'Speicherzeit/gedruckte'                              
DD0099   DC    AL1(1,09),C'Zeilentyp'                                           
*                                                                               
DD0100   DC    AL1(1,10),C'num of cis'                                          
DD0101   DC    AL1(2,09,12),C'Zln/Seite',C'Zeilen/Seite'                        
DD0102   DC    AL1(1,08),C'Kennwort'                                            
DD0103   DC    AL1(2,11,12),C'Schnitt BPZ',C'Drchschn BPZ'                      
DD0104   DC    AL1(2,07,12),C'Max BPZ',C'Max Zchn/Zle'                          
DD0105   DC    AL1(1,06),C'Aktion'                                              
DD0106   DC    AL1(1,10),C'Drucker-ID'                                          
DD0107   DC    AL1(1,11),C'Druck-Optnn'                                         
DD0108   DC    AL1(1,14),C'Drucker-Status'                                      
DD0109   DC    AL1(1,17),C'Status-nderungen'                                   
*                                                                               
DD0110   DC    AL1(1,05),C'Alpha'                                               
DD0111   DC    AL1(2,04,07),C'Rpts',C'Reports'                                  
DD0112   DC    AL1(1,10),C'live hours'                                          
DD0113   DC    AL1(1,03),C'Job'                                                 
DD0114   DC    AL1(1,06),C'Klasse'                                              
DD0115   DC    AL1(1,11),C'Bezeichnung'                                         
DD0116   DC    AL1(2,04,06),C'Frmt',C'Format'                                   
DD0117   DC    AL1(1,06),C'Kopien'                                              
DD0118   DC    AL1(1,06),C'Vorher'                                              
DD0119   DC    AL1(1,06),C'Report'                                              
*                                                                               
DD0120   DC    AL1(1,10),C'aufgelistet'                                         
DD0121   DC    AL1(1,10),C'Reportende'                                          
DD0122   DC    AL1(2,4,6),C'Frmt',C'Format'                                     
DD0123   DC    AL1(1,14),C'connect status'                                      
DD0124   DC    AL1(2,4,8),C'Term',C'Terminal'                                   
DD0125   DC    AL1(1,06),C'Nummer'                                              
DD0126   DC    AL1(1,02),C'ID'                                                  
DD0127   DC    AL1(1,06),C'System'                                              
DD0128   DC    AL1(1,08),C'Adresse'                                             
DD0129   DC    AL1(1,03),C'Typ'                                                 
*                                                                               
DD0130   DC    AL1(3,3,4,8),C'Prg',C'Prog',C'Programm'                          
DD0131   DC    AL1(1,02),C'ID'                                                  
DD0132   DC    AL1(2,06,10),C'Schirm',C'Bildschirm'                             
DD0133   DC    AL1(2,10,15),C'Cntrl Unit',C'Kontrolleinheit'                    
DD0134   DC    AL1(1,03),C'ja '                                                 
DD0135   DC    AL1(1,06),C'Selekt'                                              
DD0136   DC    AL1(1,11),C'Leitung(en)'                                         
DD0137   DC    AL1(1,10),C'Firmencode'                                          
DD0138   DC    AL1(1,07),C'Leitung'                                             
DD0139   DC    AL1(1,07),C'ºrtlich'                                             
*                                                                               
DD0140   DC    AL1(1,06),C'Bereit'                                              
DD0141   DC    AL1(1,05),C'Hilfe'                                               
DD0142   DC    AL1(1,04),C'dial'                                                
DD0143   DC    AL1(1,05),C'mdrop'                                               
DD0144   DC    AL1(1,05),C'lease'                                               
DD0145   DC    AL1(1,08),C'not open'                                            
DD0146   DC    AL1(1,10),C'up && open'                                          
DD0147   DC    AL1(1,12),C'down && open'                                        
DD0148   DC    AL1(2,09,12),C'Device addr',C'Einheitenadr'                      
DD0149   DC    AL1(2,04,08),C'Ghlt',C'Gehalten'                                 
*                                                                               
DD0150   DC    AL1(1,12),C'Zugangs-Code'                                        
DD0151   DC    AL1(1,06),C'update'                                              
DD0152   DC    AL1(1,04),C'nein'                                                
DD0153   DC    AL1(1,03),C'log'                                                 
DD0154   DC    AL1(1,03),C'OK '                                                 
DD0155   DC    AL1(1,09),C'Unbekannt'                                           
DD0156   DC    AL1(1,13),C'Sys Input No.'                                       
DD0157   DC    AL1(1,11),C'Erstes Wort'                                         
DD0158   DC    AL1(1,12),C'Zweites Wort'                                        
DD0159   DC    AL1(1,12),C'reverse mask'                                        
*                                                                               
DD0160   DC    AL1(2,09,10),C'verbinden',C'Verbindung'                          
DD0161   DC    AL1(1,07),C'violate'                                             
DD0162   DC    AL1(1,11),C'Meldungs-Nr'                                         
DD0163   DC    AL1(2,7,11),C'Term-ID',C'Terminal-ID'                            
DD0164   DC    AL1(1,08),C'Zeile(n)'                                            
DD0165   DC    AL1(1,10),C'Best{tigen'                                          
DD0166   DC    AL1(1,04),C'Text'                                                
DD0167   DC    AL1(1,07),C'Maximum'                                             
DD0168   DC    AL1(1,09),C'allgemein'                                           
DD0169   DC    AL1(1,04),C'Ende'                                                
*                                                                               
DD0170   DC    AL1(2,2,13),C'PQ',C'Warteschlange'                               
DD0171   DC    AL1(1,04),C'Stop'                                                
DD0172   DC    AL1(1,06),C'Letzte'                                              
DD0173   DC    AL1(1,04),C'Plan'                                                
DD0174   DC    AL1(1,06),C'Fehler'                                              
DD0175   DC    AL1(1,9),C'Abbrechen'                                            
DD0176   DC    AL1(2,3,8),C'Ben',C'Benutzer'                                    
DD0177   DC    AL1(1,06),C'ndern'                                              
DD0178   DC    AL1(1,05),C'Datum'                                               
DD0179   DC    AL1(1,11),C'Annullieren'                                         
*                                                                               
DD0180   DC    AL1(1,05),C'Modus'                                               
DD0181   DC    AL1(1,05),C'heute'                                               
DD0182   DC    AL1(1,07),C'Lºschen'                                             
DD0183   DC    AL1(2,4,10),C'Aktv',C'Aktivieren'                                
DD0184   DC    AL1(1,09),C'Freigeben'                                           
DD0185   DC    AL1(1,04),C'leer'                                                
DD0186   DC    AL1(1,06),C'Umfang'                                              
DD0187   DC    AL1(2,09,12),C'speichern',C'Speicherzeit'                        
DD0188   DC    AL1(1,06),C'Inhalt'                                              
DD0189   DC    AL1(1,07),C'manuell'                                             
*                                                                               
DD0190   DC    AL1(1,08),C'tempor{r'                                            
DD0191   DC    AL1(1,04),C'Oben'                                                
DD0192   DC    AL1(1,07),C'lºschen'                                             
DD0193   DC    AL1(2,07,11),C'Leitung',C'Leitungs-Nr'                           
DD0194   DC    AL1(1,08),C'gelºscht'                                            
DD0195   DC    AL1(1,04),C'live'                                                
DD0196   DC    AL1(1,04),C'dead'                                                
DD0197   DC    AL1(1,05),C'breit'                                               
DD0198   DC    AL1(1,09),C'sortieren'                                           
DD0199   DC    AL1(1,06),C'Edatum'                                              
*                                                                               
DD0200   DC    AL1(1,06),C'Ddatum'                                              
DD0201   DC    AL1(1,06),C'Sdatum'                                              
DD0202   DC    AL1(1,06),C'Rdatum'                                              
DD0203   DC    AL1(1,03),C'DDS'                                                 
DD0204   DC    AL1(1,08),C'Erstellt'                                            
DD0205   DC    AL1(1,05),C'unten'                                               
DD0206   DC    AL1(1,09),C'Suchhilfe'                                           
DD0207   DC    AL1(1,06),C'Filter'                                              
DD0208   DC    AL1(1,06),C'Zur}ck'                                              
DD0209   DC    AL1(1,16),C'Keine Positionen'                                    
*                                                                               
DD0210   DC    AL1(1,16),C'Falsche Position'                                    
DD0211   DC    AL1(1,08),C'Position'                                            
DD0212   DC    AL1(1,05),C'Suche'                                               
DD0213   DC    AL1(1,09),C'Suchhilfe'                                           
DD0214   DC    AL1(1,05),C'Liste'                                               
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
**PAN#1  DC    CL21'002SRDDGER   08/22/00'                                      
         END                                                                    
