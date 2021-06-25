*          DATA SET SPCBLLIST  AT LEVEL 080 AS OF 01/29/21                      
*PHASE T00A9EA                                                                  
*====================================================================*          
* 16MAY13 MHER DO BINSRCH FOR 3 BYTE CABLE LOOKUPS                              
*              BY CREATING SECOND SORTED TABLE IN STAPACK!                      
* 14FEB05 MHER ADD 4-BYTE NETWORK EQUIVALENCES                                  
* 22AUG01 MHER DO NOT -- UNDER ANY CIRCUMSTANCE --                              
*              PUT ANYTHING IN FRONT OF THIS TABLE                              
*              DDSTAPACK ASSUMES THAT CABLETAB IS AT THE TOP !                  
* 20MAR00 MHER CHANGE ALL COMPARE LENGTHS TO X'03' SO                *          
*             THAT FX AND LN (AMONG OTHERS) ARE DISTINGUISHABLE      *          
*====================================================================*          
                                                                                
*====================================================================*          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* BE SURE TO NOTIFY STRATA WHEN NEW ENTRIES ARE ADDED TO THIS TABLE             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*====================================================================*          
*                                                                               
***************************************                                         
**** AS FAR AS WE KNOW, THE ONLY PROGRAMS TO REFERENCE THESE TABLES ARE         
****              DDSTAVAL                                                      
****              DDSTAPACK                                                     
****              SPREPUC02                                                     
****              SPREPNC02                                                     
****              SPREPPC02                                                     
***************************************                                         
*                                                                               
*         CL3 - NETWORK CODE                                                    
*         XL2 - NTH CABLE NETWORK                                               
*         XL1 - TABLE ENTRY LENGTH - FIRST ENTRY ONLY                           
*         XL1 - FLAG BYTE (X'80'=UNUSED NETWORK)                                
*                               =TURN OFF TO ACTIVATE NETWORK                   
*                         (X'40'=TOP 24 NETWORK)                                
*                         (X'20'=NSI/FUSION EXCEPTION)                          
*                         REMAINING BITS USED FOR TOP 24 SEQ NUM                
*         CL4 - NCC NETWORK CODE                                                
***************************************                                         
T00A9EA  CSECT                                                                  
CABLETAB DS    0C                                                               
* BYTE 5 OF FIRST ENTRY DEFINES TABLE ENTRY LENGTH                              
* AFTER INITIALIZATION BY STAPACK, X'80' BIT WILL BE ON IN LENGTH               
         DC    C'AZN',X'0001',X'0B',X'00' ARIZONA NEWSCHANNEL                   
         DC    C'ZAZN'                                                          
         DC    C'ZN1',X'0002',X'00',X'00' ZONE 1 NEWS                           
         DC    C'ZON1'                                                          
AE       DC    C'AE ',X'0003',X'00',X'41' ARTS & ENTERTAINMENT                  
         DC    C'AEN '                                                          
         DC    C'ZN2',X'0004',X'00',X'00' ZONE 2 NEWS                           
         DC    C'ZON2'                                                          
         DC    C'ASP',X'0005',X'00',X'00' ARIZONA SPORTS PROGRAMMING            
         DC    C'ASPN'                                                          
         DC    C'NC8',X'0006',X'00',X'00' NEWS CH 8 WASH   <REUSED AT>          
         DC    C'LNCH'                                                          
BET      DC    C'BET',X'0007',X'00',X'42' BLACK ENTERTAINMENT TV                
         DC    C'BET '                                                          
         DC    C'ZN3',X'0008',X'00',X'00' ZONE 3 NEWS                           
         DC    C'ZON3'                                                          
         DC    C'FSW',X'0009',X'00',X'00' FOX SPORTS WEST                       
         DC    C'FSW '                                                          
         DC    C'C12',X'000A',X'00',X'00' CH 12                                 
         DC    C'CH12'                                                          
         DC    C'FSA',X'000B',X'00',X'00' FOX SPORTS ARIZONA                    
         DC    C'FSA '                                                          
         DC    C'CAN',X'000C',X'00',X'00' CANAL DE NOTICIAS                     
         DC    C'CANL'                                                          
CMT      DC    C'CMT',X'000D',X'00',X'43' COUNTRY MUSIC TELEVISION              
         DC    C'CMT '                                                          
CNB      DC    C'CNB',X'000E',X'00',X'44' CNBC                                  
         DC    C'CNBC'                                                          
CNN      DC    C'CNN',X'000F',X'00',X'45' CNN                                   
         DC    C'CNN '                                                          
COM      DC    C'COM',X'0010',X'00',X'46' COMEDY CENTRAL                        
         DC    C'CMDY'                    WAS CMD                               
         DC    C'FXA',X'0011',X'00',X'00' FOX AMERICA                           
         DC    C'FSAM'                                                          
         DC    C'COR',X'0012',X'00',X'20' TRUTV                                 
         DC    C'CRT '                                                          
         DC    C'FSN',X'0013',X'00',X'00' FOX SPORTS NETWORK                    
         DC    C'FOXS'                                                          
         DC    C'FXN',X'0014',X'00',X'00' FOX NEWS CHANNEL                      
         DC    C'FXNC'                                                          
         DC    C'EMP',X'0015',X'00',X'00' EMPIRE SPORTS NETWK (ANOTHER)         
         DC    C'EMP '                                                          
         DC    C'ESN',X'0016',X'00',X'00' EMPIRE SPORTS NETWORK                 
         DC    C'    '                                                          
ESP      DC    C'ESP',X'0017',X'00',X'47' ESPN                                  
         DC    C'ESPN'                                                          
         DC    C'ETV',X'0018',X'00',X'00' ENTERTAINMENT TELEVISION              
         DC    C'ENT '                                                          
ES2      DC    C'ES2',X'0019',X'00',X'48' ESPN2                                 
         DC    C'ESP2'                                                          
FRF      DC    C'FRF',X'001A',X'00',X'49' FREEFORM NETWORK <PREV FAM>           
         DC    C'FRFM'                    NCC CODE CHANGE JAN29/16              
         DC    C'FOX',X'001B',X'00',X'00' FOX                                   
         DC    C'FOX '                                                          
FX       DC    C'FX ',X'001C',X'00',X'4A' FX CABLE NETWORK                      
         DC    C'FX  '                                                          
         DC    C'NY1',X'001D',X'00',X'00' NY ONE NEWS  <RESUED FVC>             
         DC    C'S1NY'                    <WAS ZNY1>                            
         DC    C'GAL',X'001E',X'00',X'00' GALAVISION                            
         DC    C'GALA'                                                          
         DC    C'GEM',X'001F',X'00',X'00' GEMVISION   <NOW MUN2>                
         DC    C'GEM '                                                          
         DC    C'GOL',X'0020',X'00',X'00' GOLF CHANNEL                          
         DC    C'GOLF'                                                          
         DC    C'GAC',X'0021',X'00',X'00' GREAT AMERICAN COUNTRY                
         DC    C'GAC '                                                          
HLN      DC    C'HLN',X'0022',X'00',X'4B' HEADLINE NEWS                         
         DC    C'HLN '                                                          
         DC    C'HSE',X'0023',X'00',X'00' HOME SPORTS ENTERTAINMENT             
         DC    C'HSE '                                                          
         DC    C'HTS',X'0024',X'00',X'00' HOME TEAM SPORTS                      
         DC    C'HTS '                                                          
         DC    C'HNL',X'0025',X'00',X'00' HEADLINE NEWS LOCAL                   
         DC    C'HNLE'                                                          
HGT      DC    C'HGT',X'0026',X'00',X'4C' HOME AND GARDEN TELEVISION            
         DC    C'HGTV'                                                          
         DC    C'JCN',X'0027',X'00',X'00' JONES COMPUTER NETWORK                
         DC    C'JCN '                                                          
         DC    C'JUK',X'0028',X'00',X'00' JUKEBOX NETWORK  <REUSED ISP>         
         DC    C'JUKE'                                                          
         DC    C'RDB',X'0029',X'00',X'00' ROADBLOCK NTWK   <REUSED INF>         
         DC    C'RDBK'                                                          
         DC    C'KBL',X'002A',X'00',X'00' KBL                                   
         DC    C'KBL '                                                          
         DC    C'CNS',X'002B',X'00',X'00' CNN SI           <REUSED KCB>         
         DC    C'CNSI'                                                          
         DC    C'BHE',X'002C',X'00',X'00' BET HER             <WAS JAZ>         
         DC    C'BHER'                    NCC CODE CHANGE JAN27/18              
         DC    C'MON',X'002D',X'00',X'00' MEDIA ONE NEWS                        
         DC    C'MONE'                                                          
         DC    C'LCD',X'002E',X'00',X'00' LA CADENA DEPORTIVA                   
         DC    C'    '                                                          
LIF      DC    C'LIF',X'002F',X'00',X'4D' LIFETIME                              
         DC    C'LIF '                                                          
         DC    C'LN ',X'0030',X'00',X'00' LN                                    
         DC    C'LN  '                                                          
         DC    C'LO ',X'0031',X'00',X'00' LOCAL ORIGIN                          
         DC    C'LO  '                                                          
         DC    C'MNB',X'0032',X'00',X'00' MICROSOFT NATIONAL BDCSTG             
         DC    C'MNBC'                                                          
         DC    C'MIA',X'0033',X'00',X'00' MIAVISION LOCAL SPANISH CH            
         DC    C'MIA '                                                          
         DC    C'MSC',X'0034',X'00',X'00' MIDWEST SPORTS CHANNEL                
         DC    C'MSC '                                                          
         DC    C'MSG',X'0035',X'00',X'00' MADISON SQUARE GARDEN NETWORK         
         DC    C'MSG '                                                          
MTV      DC    C'MTV',X'0036',X'00',X'4E' MUSIC TELEVISION                      
         DC    C'MTV '                                                          
         DC    C'FNW',X'0037',X'00',X'00' ROOT SPORTS NORTHWEST                 
         DC    C'RTNW'                    WAS FSNW                              
         DC    C'MVC',X'0038',X'00',X'00' MIAMI VALLEY CHANNEL                  
         DC    C'MVC '                                                          
         DC    C'NAT',X'0039',X'00',X'00' NORTH AMERICAN TV                     
         DC    C'NATV'                                                          
         DC    C'NES',X'003A',X'00',X'00' NEW ENGLAND SPORTS NETWORK            
         DC    C'NESN'                                                          
         DC    C'NEW',X'003B',X'00',X'00' NEWS                                  
         DC    C'NEWS'                                                          
NIC      DC    C'NIC',X'003C',X'00',X'4F' NICKELODEON                           
         DC    C'NICK'                                                          
         DC    C'FSS',X'003D',X'00',X'00' FOX SPORTS NTWK SOUTHWEST             
         DC    C'FSS '                                                          
TBS      DC    C'TBS',X'003E',X'00',X'50' TURNER BRDCT SYS <REUSED NOS>         
         DC    C'TBSC'                                                          
         DC    C'CNI',X'003F',X'00',X'00' CNN INTL         <REUSED NT >         
         DC    C'CNNI'                    WAS CNI                               
         DC    C'NBC',X'0040',X'00',X'00' NATIONAL BROADCAST COMPANY            
         DC    C'NBC '                                                          
         DC    C'OCN',X'0041',X'00',X'00' OCEAN COUNTY NEWS                     
         DC    C'LOCN'                                                          
         DC    C'HAL',X'0042',X'00',X'00' ODYSSEY IS NOW HALLMARK               
         DC    C'HALL'                                                          
         DC    C'OLC',X'0043',X'00',X'00' OUTDOOR CHANNEL                       
         DC    C'OUTD'                                                          
         DC    C'PAD',X'0044',X'00',X'00' PADR                                  
         DC    C'PADR'                                                          
         DC    C'PAS',X'0045',X'00',X'00' PASS                                  
         DC    C'PASS'                                                          
         DC    C'PRT',X'0046',X'00',X'00' PRIME TICKET                          
         DC    C'    '                                                          
         DC    C'PRV',X'0047',X'00',X'00' PREVIEW GUIDE                         
         DC    C'PRV '                                                          
PSN      DC    C'PSN',X'0048',X'00',X'51' PRIME SPORTS NETWORK                  
         DC    C'PSN '                                                          
         DC    C'FSD',X'0049',X'00',X'00' FOX SPOTS DETROIT<REUSED GUI>         
         DC    C'FSD '                                                          
         DC    C'TKC',X'004A',X'00',X'00' TIMEWARNR KC        <WAS MET>         
         DC    C'SPKC'                                                          
         DC    C'QVC',X'004B',X'00',X'00' SHOPPING CHANNEL                      
         DC    C'QVC '                                                          
         DC    C'ANP',X'004C',X'00',X'00' ANMIMAL PLANET                        
         DC    C'APL '                                                          
         DC    C'BAY',X'004D',X'00',X'00' BAY TV NEWS                           
         DC    C'BN9 '                                                          
         DC    C'FS1',X'004E',X'00',X'00' FOX SPORTS 1        <WAS SPD>         
         DC    C'FS1 '                                                          
         DC    C'CNE',X'004F',X'00',X'00' CNN EN ESPANOL                        
         DC    C'CNNE'                                                          
         DC    C'DLF',X'0050',X'00',X'00' DISCOVERY LIFE      <WAS FIT>         
         DC    C'DLIF'                    NCC CODE CHANGE JAN23/15              
         DC    C'SCN',X'0051',X'00',X'00' SAN DIEGO NEWS                        
         DC    C'SDNC'                                                          
         DC    C'SCC',X'0052',X'00',X'00' SPORTSCHANNEL CHICAGO                 
         DC    C'SCC '                                                          
         DC    C'CXS',X'0053',X'00',X'00' COX SPORTS CHANNEL                    
         DC    C'CXSP'                                                          
         DC    C'SCF',X'0054',X'00',X'00' SCIFI NETWORK                         
         DC    C'SYFY'                                                          
SPC      DC    C'SPC',X'0055',X'00',X'52' SPORTSCHANNEL                         
         DC    C'SPCH'                                                          
         DC    C'SSN',X'0056',X'00',X'00' SPORTS SOUTH NETWORK                  
         DC    C'    '                                                          
         DC    C'SUN',X'0057',X'00',X'00' SUNSHINE NETWORK                      
         DC    C'SUN '                                                          
         DC    C'SPO',X'0058',X'00',X'00' REGIONAL SPORTS                       
         DC    C'    '                                                          
         DC    C'SPF',X'0059',X'00',X'00' SPORTSCHANNEL FLORIDA                 
         DC    C'SCF '                                                          
         DC    C'SPN',X'005A',X'00',X'00' SPORTSCHANNEL NEW YORK                
         DC    C'SCNY'                                                          
         DC    C'TVL',X'005B',X'00',X'00' NICKELODEON TV LAND                   
         DC    C'TVL '                                                          
         DC    C'SCA',X'005C',X'00',X'00' SPORTS CH PACIFIC                     
         DC    C'SCPF'                                                          
         DC    C'NBY',X'005D',X'00',X'00' NBC SPORTS BAY AREA <WAS FSB>         
         DC    C'NSBA'                                                          
         DC    C'TLC',X'005E',X'00',X'00' THE LEARNING CHANNEL                  
         DC    C'TLC '                                                          
TDC      DC    C'TDC',X'005F',X'00',X'53' THE DISCOVERY CHANNEL                 
         DC    C'DISC'                                                          
         DC    C'TEL',X'0060',X'00',X'00' TELEMUNDO                             
         DC    C'TMND'                    WAS TMD                               
TNN      DC    C'TNN',X'0061',X'00',X'54' TNN                                   
         DC    C'TNN '                                                          
TNT      DC    C'TNT',X'0062',X'00',X'55' TNT                                   
         DC    C'TNT '                                                          
         DC    C'TOO',X'0063',X'00',X'00' CARTOON CHANNEL                       
         DC    C'TOON'                                                          
         DC    C'TTC',X'0064',X'00',X'00' THE TRAVEL CHANNEL                    
         DC    C'TRAV'                                                          
TWC      DC    C'TWC',X'0065',X'00',X'56' THE WEATHER CHANNEL                   
         DC    C'TWC '                                                          
         DC    C'FOO',X'0066',X'00',X'00' FOOD NETWORK                          
         DC    C'FOOD'                                                          
         DC    C'THC',X'0067',X'00',X'00' THE HISTORY CHANNEL                   
         DC    C'HIST'                                                          
         DC    C'UNI',X'0068',X'00',X'00' UNIVISION                             
         DC    C'UNIV'                    WAS UNV                               
USA      DC    C'USA',X'0069',X'00',X'57' USA                                   
         DC    C'USA '                                                          
         DC    C'FSC',X'006A',X'00',X'00' FOX SPRT CHICAGO                      
         DC    C'FSCH'                                                          
VH1      DC    C'VH1',X'006B',X'00',X'58' VIDEO HITS                            
         DC    C'VH1 '                                                          
         DC    C'SDN',X'006C',X'00',X'00' SAN DIEGO NEWSCHANNEL 15              
         DC    C'    '                                                          
         DC    C'FSM',X'006D',X'00',X'00' FOX SPORTS MIDWEST                    
         DC    C'FSMW'                                                          
         DC    C'APT',X'006E',X'00',X'00' AT&T SPRTS PITTSBURG<WAS FSP>         
         DC    C'ATPT'                    NCC CODE CHANGE JAN27/18              
         DC    C'ARM',X'006F',X'00',X'00' AT&T SPORTS ROCKY MT<WAS FSR>         
         DC    C'ATRM'                    NCC CODE CHANGE JAN27/18              
         DC    C'WOR',X'0070',X'00',X'00' WOR                                   
         DC    C'WOR '                                                          
         DC    C'WSN',X'0071',X'00',X'00' WISCONSIN SPORTS NETWORK              
         DC    C'WSN '                                                          
         DC    C'WBN',X'0072',X'00',X'00' WARNER BROTHERS NETWORK               
         DC    C'WBN '                                                          
         DC    C'FSO',X'0073',X'00',X'00' FOX SPORTS SOUTH                      
         DC    C'FSSO'                                                          
         DC    C'FS2',X'0074',X'00',X'00' FOX SPORTS PRIME TICKET               
         DC    C'FSPT'                                                          
         DC    C'SCO',X'0075',X'00',X'00' SPORTS CHANNEL OHIO                   
         DC    C'SCO '                                                          
         DC    C'SCP',X'0076',X'00',X'00' SPORTS CHANNEL PLUS                   
         DC    C'SCP '                                                          
         DC    C'SPB',X'0077',X'00',X'00' SPORTS CHANNEL BAY AREA               
         DC    C'SCB '                                                          
         DC    C'SPP',X'0078',X'00',X'00' SPORTS CH PHILADELPHIA                
         DC    C'SPH '                                                          
         DC    C'CCP',X'0079',X'00',X'00' SPORTS CH CHICAGO PLUS                
         DC    C'    '                                                          
         DC    C'SPE',X'007A',X'00',X'00' SPORTS CHANNEL NEW ENGLAND            
         DC    C'SCNE'                                                          
         DC    C'KNO',X'007B',X'00',X'00' KNOWLEDGE NETWORK                     
         DC    C'KNOW'                                                          
         DC    C'PPV',X'007C',X'00',X'00' PAY-PER-VIEW                          
         DC    C'PPV '                                                          
         DC    C'COX',X'007D',X'00',X'00' COX SPORTS PHOENIX                    
         DC    C'COXS'                                                          
         DC    C'GAM',X'007E',X'00',X'00' GAME SHOW NETWORK                     
         DC    C'GSN '                                                          
         DC    C'CAS',X'007F',X'00',X'00' CASA                                  
         DC    C'CASA'                                                          
         DC    C'NWA',X'0080',X'00',X'00' NBC SPORTS WA       <WAS SNE>         
         DC    C'NSWA'                    NCC CODE CHANGE JAN27/18              
         DC    C'FNE',X'0081',X'00',X'00' FOX SPORTS NEW ENGLAND                
         DC    C'FSNE'                                                          
         DC    C'FNY',X'0082',X'00',X'00' FOX SPORTS NEW YORK                   
         DC    C'MSGP'                                                          
         DC    C'FXO',X'0083',X'00',X'00' FOX SPORTS OHIO                       
         DC    C'FSOH'                                                          
         DC    C'ZN4',X'0084',X'00',X'00' ZONE 4 NEWS                           
         DC    C'ZON4'                                                          
         DC    C'ZN5',X'0085',X'00',X'00' ZONE 5 NEWS                           
         DC    C'ZON5'                                                          
         DC    C'ZN6',X'0086',X'00',X'00' ZONE 6 NEWS                           
         DC    C'ZON6'                                                          
         DC    C'BOX',X'0087',X'00',X'00' THE VIDEO JUKEBOX                     
         DC    C'BOX '                                                          
         DC    C'SNP',X'0088',X'00',X'00' SNEAK PREVIEW                         
         DC    C'SPRV'                                                          
         DC    C'CNF',X'0089',X'00',X'00' CNN FINANCIAL NEWS                    
         DC    C'CNFN'                                                          
         DC    C'AAT',X'008A',X'00',X'00' ASIAN AMERICAN TV                     
         DC    C'AATV'                                                          
         DC    C'AHN',X'008B',X'00',X'00' AMER HEALTH NTWK <NOW DHLT>           
         DC    C'AHN '                                                          
         DC    C'CSN',X'008C',X'00',X'00' CLASSIC SPORTS CHANNEL                
         DC    C'CLAS'                                                          
         DC    C'KAL',X'008D',X'00',X'00' KALEIDOSCOPE                          
         DC    C'KALI'                                                          
         DC    C'OLN',X'008E',X'00',X'00' OUTDOOR LIFE NETWORK                  
         DC    C'OLN '                                                          
         DC    C'UPN',X'008F',X'00',X'00' UNITED PARAMOUNT NETWORK              
         DC    C'UPN '                                                          
         DC    C'NEC',X'0090',X'00',X'00' NEW ENGLAND CABLE NEWS                
         DC    C'NECN'                                                          
         DC    C'TWP',X'0091',X'00',X'00' TIME WARNER PROGRAMMING NTWK          
         DC    C'TWPN'                                                          
         DC    C'TRC',X'0092',X'00',X'00' LOCAL WEATHER RADAR TRACK             
         DC    C'TRCK'                                                          
         DC    C'LWW',X'0093',X'00',X'00' NEWS WATCH CHANNEL 15                 
         DC    C'LWWL'                                                          
         DC    C'AMC',X'0094',X'00',X'00' AMERICA MOVIE CLASSIC                 
         DC    C'AMC '                                                          
         DC    C'WOL',X'0095',X'00',X'00' CHICAGO WOLVES HOCKEY NTWK            
         DC    C'WOLF'                                                          
         DC    C'KAK',X'0096',X'00',X'00' ABC CH 51                             
         DC    C'KAKE'                                                          
         DC    C'BRV',X'0097',X'00',X'00' BRAVO                                 
         DC    C'BRVO'                                                          
         DC    C'PAX',X'0098',X'00',X'00' PAX                                   
         DC    C'PAX '                                                          
         DC    C'OSN',X'0099',X'00',X'00' OCEAN STATE NTWK    <WAS RIN>         
         DC    C'OSN '                    NCC CODE CHANGE FEB0/21               
         DC    C'GTV',X'009A',X'00',X'00' GOODLIFE TELEVISION NETWORK           
         DC    C'GTV '                                                          
         DC    C'ZDT',X'009B',X'00',X'00' ZIFF DAVIS TV                         
         DC    C'ZDTV'                                                          
         DC    C'POP',X'009C',X'00',X'00' POP FULL SCREEN     <WAS TVG>         
         DC    C'POP '                    NCC CODE CHANGE JAN23/15              
         DC    C'BY9',X'009D',X'00',X'00' TAMPA BAY NEWS 9                      
         DC    C'S1TP'                    <WAS ZBN9>                            
         DC    C'CX3',X'009E',X'00',X'00' COX SPORTSCHANNEL CHANNEL 3           
         DC    C'COX3'                                                          
         DC    C'RSO',X'009F',X'00',X'00' RED SOX CHANNEL                       
         DC    C'RSOX'                                                          
         DC    C'NWC',X'00A0',X'00',X'00' NORTH WEST NEWS CHANNEL               
         DC    C'NWCN'                                                          
         DC    C'FSU',X'00A1',X'00',X'20' FSN SPORSOUTH       <WAS TSO>         
         DC    C'FSSE'                    NCC CODE CHANGE JAN29/16              
         DC    C'CH1',X'00A2',X'00',X'00' MEDIA 1 CHANNEL 3                     
         DC    C'MO3 '                                                          
         DC    C'LNC',X'00A3',X'00',X'00' LOCAL NEWS ON CABLE                   
         DC    C'ZLNC'                                                          
         DC    C'DSN',X'00A4',X'00',X'00' DISNEY CHANNEL                        
         DC    C'DSNY'                                                          
         DC    C'FXS',X'00A5',X'00',X'00' FOX SPORT WORLD                       
         DC    C'FSWD'                                                          
         DC    C'CMK',X'00A6',X'00',X'00' CABLE MARKETPLACE                     
         DC    C'CMKT'                                                          
         DC    C'ESQ',X'00A7',X'00',X'00' ESQUIRE             <WAS STY>         
         DC    C'ESQ '                                                          
         DC    C'ECL',X'00A8',X'00',X'00' ESPN CLASSIC                          
         DC    C'ESCL'                                                          
         DC    C'CN8',X'00A9',X'00',X'00' SONY ENT                              
         DC    C'SETV'                    WAS CN8                               
         DC    C'HSN',X'00AA',X'00',X'00' HOME SHOPPING NETWORK                 
         DC    C'HSN '                                                          
         DC    C'HLT',X'00AB',X'00',X'00' DISCOVERY HEALTH NETWORK              
         DC    C'DHLT'                                                          
         DC    C'RNW',X'00AC',X'00',X'00' ROCHESTER LOCAL NEWS                  
         DC    C'RNWS'                                                          
         DC    C'DSK',X'00AD',X'00',X'00' DISCOVERY KIDS LATIN AMERICA          
         DC    C'DKID'                                                          
         DC    C'FSE',X'00AE',X'00',X'00' FOX SPORTS WORLD EN ESPANOL           
         DC    C'FOXD'                        WAS <FSE>                         
         DC    C'OXY',X'00AF',X'00',X'00' OXYGEN                                
         DC    C'OXYG'                                                          
         DC    C'FNN',X'00B0',X'00',X'00' FINANCIAL NEWS NETWORK                
         DC    C'FNN '                                                          
         DC    C'CLT',X'00B1',X'00',X'00' CHICAGOLAND TV                        
         DC    C'CLTV'                        WAS <LCLT>                        
         DC    C'FXR',X'00B2',X'00',X'00' FXM RETRO           <WAS FXM>         
         DC    C'FXMR'                    NCC CODE CHANGE FEB03/17              
         DC    C'TCM',X'00B3',X'00',X'00' TURNER CLASSIC MOVIES                 
         DC    C'TCM '                                                          
         DC    C'OVT',X'00B4',X'00',X'00' OVATION                               
         DC    C'OVTN'                    WAS OVA                               
         DC    C'FSF',X'00B5',X'00',X'00' FOX SPORTS FLA SENT AS FSFL           
         DC    C'FSFL'                                                          
         DC    C'ENN',X'00B6',X'00',X'00' ESPNEWS                               
         DC    C'ENN '                                                          
         DC    C'MTR',X'00B7',X'00',X'00' METRO GUIDE                           
         DC    C'MTRG'                                                          
         DC    C'AUN',X'00B8',X'00',X'00' NEWS 8 AUSTIN                         
         DC    C'NWS8'                                                          
         DC    C'T33',X'00B9',X'00',X'00' TV33                                  
         DC    C'TV33'                                                          
         DC    C'SOP',X'00BA',X'00',X'00' SOAP                                  
         DC    C'SOAP'                                                          
         DC    C'AJS',X'00BB',X'00',X'00' AM JEWLERY STORE                      
         DC    C'AJS '                                                          
         DC    C'ATN',X'00BC',X'00',X'00' AMERICA'S TALKING NETWORK             
         DC    C'ATN '                                                          
         DC    C'FNC',X'00BD',X'00',X'00' FLORIDA NEWS CHANNEL                  
         DC    C'FLNC'                                                          
         DC    C'TEC',X'00BE',X'00',X'00' TECH TV                               
         DC    C'TECH'                                                          
         DC    C'ATT',X'00BF',X'00',X'00' ATT3                                  
         DC    C'AT&&T'                                                         
         DC    C'NWT',X'00C0',X'00',X'00' NEWS 12 LONG ISLAND                   
         DC    C'NWT '                        WAS <N12 >                        
         DC    C'STC',X'00C1',X'00',X'00' ST. CLOUD STATE UNIVERSITY            
         DC    C'    '                                                          
         DC    C'MAS',X'00C2',X'00',X'00' MAS! ARIZONA                          
         DC    C'MAS '                                                          
         DC    C'TCC',X'00C3',X'00',X'00' THE COMEDY CHANNEL                    
         DC    C'TCC '                                                          
         DC    C'CAB',X'00C4',X'00',X'00' CABLE LOCAL SPORTS (CH 65)            
         DC    C'CABL'                                                          
         DC    C'ZRI',X'00C5',X'00',X'00' NEWS 12 PLUS RING                     
         DC    C'ZNP+'                    WAS <ZTWC>                            
         DC    C'NOS',X'00C6',X'00',X'00' NOSTAGLIA                             
         DC    C'NOS '                                                          
         DC    C'CFN',X'00C7',X'00',X'00' CENTRAL FLORIDA NEWS 13               
         DC    C'S1OR'                    <WAS ZCFN'                            
         DC    C'LC5',X'00C8',X'00',X'00' NEWS CHANNEL 5 PLUS                   
         DC    C'LNC5'                                                          
         DC    C'LV1',X'00C9',X'00',X'00' LAS VEGAS ONE NEWS                    
         DC    C'LV1 '                                                          
         DC    C'MSN',X'00CA',X'00',X'00' MIDSOUTH NEWS NET                     
         DC    C'NC3A'                                                          
         DC    C'N53',X'00CB',X'00',X'00' NEWS NOW 53                           
         DC    C'ZN53'                                                          
         DC    C'NO1',X'00CC',X'00',X'00' NEWS ON ONE                           
         DC    C'N4U '                                                          
         DC    C'NBX',X'00CD',X'00',X'00' NEWS 12 BRONX                         
         DC    C'NWBX'                                                          
         DC    C'NCT',X'00CE',X'00',X'00' NEWS 12 CONNECTICUT                   
         DC    C'NWCT'                                                          
         DC    C'NNJ',X'00CF',X'00',X'00' NEWS 12 NEW JERSEY                    
         DC    C'NWNJ'                                                          
         DC    C'NWW',X'00D0',X'00',X'00' NEWS 12 WESTCHESTER                   
         DC    C'NWWC'                                                          
         DC    C'ONN',X'00D1',X'00',X'00' OHIO NEWS NET                         
         DC    C'ONN '                                                          
         DC    C'TXC',X'00D2',X'00',X'00' TEXAS CABLE NEWS                      
         DC    C'TXCN'                                                          
         DC    C'WRN',X'00D3',X'00',X'00' WRNN                                  
         DC    C'WRNN'                                                          
         DC    C'PCN',X'00D4',X'00',X'00' PITTSBURG CABLE NEWS                  
         DC    C'PCNC'                    WAS LPCN                              
         DC    C'TDS',X'00D5',X'00',X'00' DISNEY DXD                            
         DC    C'DXD '                                                          
         DC    C'LMN',X'00D6',X'00',X'00' LIFETIME MOVIE NETWORK                
         DC    C'LMN '                                                          
         DC    C'KUS',X'00D7',X'00',X'00' SAN DIEGO 4                           
*        DC    C'SD4 '                    NCC CODE CHANGE JAN23/15              
         DC    C'KUSI'                    LINK BACK TO KUSI - SPEC5614          
         DC    C'TG2',X'00D8',X'00',X'00' TVG2                <WAS HRN>         
         DC    C'TVG2'                    NCC CODE CHANGE JAN29/16              
         DC    C'MUC',X'00D9',X'00',X'00' MUCH MUSIC                            
         DC    C'MUCH'                                                          
         DC    C'ESD',X'00DA',X'00',X'00' ESPN DEPORTES                         
         DC    C'ESPD'                                                          
         DC    C'BBA',X'00DB',X'00',X'00' BBC AMERICA                           
         DC    C'BBCA'                                                          
         DC    C'BTV',X'00DC',X'00',X'00' BLOOMBERG TV                          
         DC    C'BTV '                                                          
         DC    C'TDI',X'00DD',X'00',X'00' SCIENCE                               
         DC    C'SCI '                    WAS <DSCI>                            
         DC    C'TDW',X'00DE',X'00',X'00' DISCOVERY WINGS                       
         DC    C'DWNG'                                                          
         DC    C'M2 ',X'00DF',X'00',X'00' MTV2                                  
         DC    C'MTV2'                                                          
         DC    C'YVA',X'00E0',X'00',X'00' YURVIEW ARIZONA <PREV CXX>            
         DC    C'YVAZ'                    NCC CODE CHANGE JAN29/16              
         DC    C'MTL',X'00E1',X'00',X'00' MTV LATIN                             
         DC    C'MTVS'                                                          
         DC    C'CH8',X'00E2',X'00',X'00' CHANNEL 8 KVBA                        
         DC    C'CH8 '                                                          
         DC    C'TIN',X'00E3',X'00',X'00' TELEMUNDO INTERNATIONAL               
         DC    C'TINT'                                                          
         DC    C'NGC',X'00E4',X'00',X'00' NATIONAL GEOGRAPHIC CHANNEL           
         DC    C'NGC '                                                          
         DC    C'UVO',X'00E5',X'00',X'00' NBC UNIVERSO  <PREV MUN>              
         DC    C'UVSO'                    NCC CODE CHANGE JAN29/16              
         DC    C'FST',X'00E6',X'00',X'00' FOX SPORTS NET - NORTH                
         DC    C'FSNO'                                                          
         DC    C'FSI',X'00E7',X'00',X'00' FOX SPORTS NET - WISCONSIN            
         DC    C'FSWI'                                                          
         DC    C'PTL',X'00E8',X'00',X'00' PORTLAND LONG FORMAT                  
         DC    C'PTLD'                                                          
         DC    C'FYI',X'00E9',X'00',X'00' FYI                 <WAS BIO>         
         DC    C'FYI '                    NCC CODE CHANGE JAN23/15              
* NCC DOES NOT SUPPORT NEXT ENTRY, BUT LCI IS BUYING IT                         
         DC    C'KAW',X'00EA',X'00',X'00' LCI CONVERSIONI !                     
         DC    C'    '                                                          
         DC    C'T13',X'00EB',X'00',X'00' TV13                                  
         DC    C'TV13'                                                          
         DC    C'BYS',X'00EC',X'00',X'00' BAY NEWS 9 EN ESPANOL                 
         DC    C'B9ES'                                                          
         DC    C'WE ',X'00ED',X'00',X'00' WETV                                  
         DC    C'WETV'                    NCC CODE CHANGE JAN23/15              
         DC    C'INT',X'00EE',X'00',X'00' INTERNATIONAL CHANNEL                 
         DC    C'INTL'                                                          
         DC    C'LWC',X'00EF',X'00',X'00' LOCAL WEATHER STATION                 
         DC    C'LWS '                                                          
         DC    C'VIC',X'00F0',X'00',X'00' VICELAND            <WAS H2 >         
         DC    C'VICE'                    NCC CODE CHANGE FEB03/17              
         DC    C'TDE',X'00F1',X'00',X'00' TOON DISNEY EN ESPANOL                
         DC    C'    '                    <WAS TDES>                            
         DC    C'YES',X'00F2',X'00',X'00' YANKEE ENTERT. SPORTS NETWK           
         DC    C'YES '                                                          
         DC    C'ME2',X'00F3',X'00',X'00' METRO STORIES CHANNEL                 
         DC    C'MET2'                                                          
* NCC DOES NOT SUPPORT NEXT ENTRY BUT LCI IS BUYING IT !                        
         DC    C'ASN',X'00F4',X'00',X'00' ACTION SPORTS (LCI CONV)              
         DC    C'    '                                                          
         DC    C'DIY',X'00F5',X'00',X'00' DO IT YOURSELF NTWK                   
         DC    C'DIY '                                                          
         DC    C'NW9',X'00F6',X'00',X'00' NEWS 9 SAN ANTONIO                    
         DC    C'NW9 '                      <WAS NSA9>                          
         DC    C'SNN',X'00F7',X'00',X'00' SNN NEWS 6                            
         DC    C'SNN6'                                                          
         DC    C'LRW',X'00F8',X'00',X'00' LIFETIME REAL WOMEN                   
         DC    C'LRWM'                                                          
         DC    C'FRP',X'00F9',X'00',X'00' FOX SPORTS NET - ROHNERT PARK         
         DC    C'FSRP'                                                          
         DC    C'N24',X'00FA',X'00',X'00' NEWS 24 HOUSTON                       
         DC    C'NW24'                                                          
         DC    C'CCT',X'00FB',X'00',X'00' CHARTER COMM. INFO. NETWORK           
         DC    C'CCIN'                       <USED TO BE CCTV>                  
         DC    C'N14',X'00FC',X'00',X'00' NEWS CAROLINA 14                      
         DC    C'NC14'                                                          
         DC    C'O16',X'00FD',X'00',X'00' SPECTRUM OC16  <NO DS CHANGE>         
         DC    C'SC16'                    NCC CODE CHANGE JAN25/19              
         DC    C'CSS',X'00FE',X'00',X'00' COMCAST/CHARTER SPORTS SE             
         DC    C'CSS '                                                          
         DC    C'IC6',X'00FF',X'00',X'00' LOCAL NEWS/TRAFFC NORTHERN KY         
         DC    C'ICN6'                                                          
         DC    C'N9S',X'0100',X'00',X'00' NEWS 9 SAN ANTONIO                    
         DC    C'NSA9'                       <NOW NW9>                          
         DC    C'DHO',X'0101',X'00',X'00' DISCOVERY HOME & LEISURE              
         DC    C'GRN '             USED TO BE DHOM                              
         DC    C'SIV',X'0102',X'00',X'00' SINOVISION                            
         DC    C'SINO'                                                          
         DC    C'CXT',X'0103',X'00',X'00' COX SPORTS TELEVISION 37              
         DC    C'CST '                                                          
         DC    C'FUS',X'0104',X'00',X'00' FUSE, FORMER MUC - MUCH MUSIC         
         DC    C'FUSE'                                                          
         DC    C'BLZ',X'0105',X'00',X'00' BLAZERVISION                          
         DC    C'BLZ '                                                          
         DC    C'NYN',X'0106',X'00',X'00' NEW YORK 1 NOTICIAS                   
         DC    C'S1NT'                    <WAS NOT1>                            
         DC    C'PAR',X'0107',X'00',X'00' PARAMOUNT           <WAS SPK>         
         DC    C'PAR '                    NCC CODE CHANGE JAN27/18              
         DC    C'KBE',X'0108',X'00',X'00' UPN AFFIL AUSTN,SAN ANTONIO           
         DC    C'KBEJ'                                                          
         DC    C'BSN',X'0109',X'00',X'00' BIG SKY NETWORK                       
         DC    C'BSN '                                                          
         DC    C'JNW',X'010A',X'00',X'00' JAMESTOWN NEWS 8                      
         DC    C'JNW8'                                                          
         DC    C'TWS',X'010B',X'00',X'00' TIME WARNER SPORTS CHANNEL            
         DC    C'SPSS'                                                          
         DC    C'WB5',X'010C',X'00',X'00' CAPS & WIZARDS                        
         DC    C'WB50'                                                          
         DC    C'VSN',X'010D',X'00',X'00' VICTORY SPORTS NETWORK                
         DC    C'VSN '                                                          
         DC    C'KN2',X'010E',X'00',X'00' KANSAS NOW 22                         
         DC    C'KN22'                                                          
         DC    C'REC',X'010F',X'00',X'00' THE REAL ESTATE CHANNEL               
         DC    C'REC '                                                          
         DC    C'TV2',X'0110',X'00',X'00' TV2 - LOCAL PROGRAMMING               
         DC    C'TV2 '                                                          
         DC    C'FIN',X'0111',X'00',X'00' FINE LIVING                           
         DC    C'FINE'                                                          
         DC    C'RCN',X'0112',X'00',X'00' RCN8 LOCAL NEWS                       
         DC    C'RCN8'                                                          
         DC    C'FXW',X'0113',X'00',X'00' FOX SPORTS WASHINGTON (FSWA)          
         DC    C'FSWA'                                                          
         DC    C'NSY',X'0114',X'00',X'00' NEWS 10 SYRACUSE                      
         DC    C'NW10'                                                          
         DC    C'LCN',X'0115',X'00',X'00' CAPITAL NEWS 9 ALBANY                 
         DC    C'LCN9'                                                          
         DC    C'EVN',X'0116',X'00',X'00' EVENT NETWORK                         
         DC    C'EVNT'                                                          
         DC    C'TV1',X'0117',X'00',X'00' TV ONE                                
         DC    C'TV1 '                                                          
         DC    C'SD4',X'0118',X'00',X'00' SAN DIEGO CHANNEL 4                   
         DC    C'SD4 '                                                          
         DC    C'WSC',X'0119',X'00',X'00' WEATHERSCAN                           
         DC    C'WSCN'                                                          
         DC    C'TWT',X'011A',X'00',X'00' SPECTRUM TV                           
         DC    C'SPTV'                    WAS TWTV - NCC CHG FEB01/21           
         DC    C'CNW',X'011B',X'00',X'00' COMCAST NORTH WEST 14                 
         DC    C'CNW '                                                          
         DC    C'WWW',X'011C',X'00',X'00' WORLDWIDEWEB.COM                      
         DC    C'WWW.'                                                          
         DC    C'BRW',X'011D',X'00',X'00' BRAVO WEST                            
         DC    C'BRVW'                                                          
         DC    C'DSP',X'011E',X'00',X'00' DISCOVERY EN ESPANOL                  
         DC    C'DSE '                    WAS DSPA                              
         DC    C'DST',X'011F',X'00',X'00' INVESTIGATION DISCOVERY               
         DC    C'ID  '                                                          
         DC    C'HSW',X'0120',X'00',X'00' HISTORY CHANNEL WEST                  
         DC    C'HISW'                                                          
         DC    C'HGW',X'0121',X'00',X'00' HOME AND GARDEN TV WEST               
         DC    C'HGTW'                                                          
         DC    C'HTV',X'0122',X'00',X'00' HTV MUSICA                            
         DC    C'HTV '                    WAS HTVM                              
         DC    C'MBC',X'0123',X'00',X'00' MBC NETWORK                           
         DC    C'MBCN'                                                          
         DC    C'NWI',X'0124',X'00',X'00' NEWSWORLD INTERNATIONAL               
         DC    C'NWI '                                                          
         DC    C'TSC',X'0125',X'00',X'00' SCIENCE CHANNEL                       
         DC    C'DSCI'                                                          
         DC    C'TRI',X'0126',X'00',X'00' TRIO                                  
         DC    C'TRIO'                                                          
         DC    C'FXL',X'0127',X'00',X'00' FOX LIFE                              
         DC    C'FOXL'                                                          
         DC    C'WSD',X'0128',X'00',X'00' WISDOM CHANNEL                        
         DC    C'WSDM'                                                          
         DC    C'TRA',X'0129',X'00',X'00' WEATHER TRACKER                       
         DC    C'TRAC'                                                          
         DC    C'AZT',X'012A',X'00',X'00' AZTECA AMERICA                        
         DC    C'AZAN'                                                          
         DC    C'AAB',X'012B',X'00',X'00' ABC 7/MONTEREY-SALINAS                
         DC    C'AABC'                                                          
         DC    C'FLC',X'012C',X'00',X'00' FALCONVISION (FALCONS GAMES)          
         DC    C'FLCN'                                                          
         DC    C'ALT',X'012D',X'00',X'00' ALTITUDE NETWORK                      
         DC    C'ALT '                                                          
         DC    C'NCH',X'012E',X'00',X'00' NBC SPORTS CHICAGO  <WAS CSC>         
         DC    C'NSCH'                    NCC CODE CHANGE JAN27/18              
         DC    C'DCC',X'012F',X'00',X'00' DALLAS COWBOYS CHANNEL                
         DC    C'DCC '                                                          
         DC    C'N18',X'0130',X'00',X'00' CABLEVISION NEWS 18                   
         DC    C'N18W'                                                          
         DC    C'NFL',X'0131',X'00',X'00' NFL NETWORK                           
         DC    C'NFLN'                                                          
         DC    C'CLD',X'0132',X'00',X'00' COMCAST LOCAL - DETROIT               
         DC    C'CLD '                                                          
         DC    C'CLF',X'0133',X'00',X'00' COMCAST LOCAL - FLINT/SAGINAW         
         DC    C'CLF '                                                          
         DC    C'G4T',X'0134',X'00',X'00' G4TECHTV                              
         DC    C'G4  '                                                          
         DC    C'HSP',X'0135',X'00',X'00' HISTORY CHANNEL EN ESPANOL            
         DC    C'HSTE'                       WAS <HESP>                         
         DC    C'INP',X'0136',X'00',X'00' INPO - TV MART                        
         DC    C'INPO'                                                          
         DC    C'CMC',X'0137',X'00',X'00' CALIFORNIA MUSIC CHANNEL              
         DC    C'CMC '                    WAS CMCC - NCC JAN27/18               
         DC    C'CMB',X'0138',X'00',X'00' CMC - BEAT LOUNGE                     
         DC    C'CMCB'                                                          
         DC    C'CMU',X'0139',X'00',X'00' COUNTRY MUSIC CHANNEL - USA           
         DC    C'CMCU'                                                          
         DC    C'NSC',X'013A',X'00',X'00' NBC SPORTS CA       <WAS CSW>         
         DC    C'NSCA'                    NCC CODE CHANGE JAN27/18              
         DC    C'BCS',X'013B',X'00',X'00' BUCKEYE CABLE SPORTS                  
         DC    C'BCSN'                                                          
         DC    C'BFC',X'013C',X'00',X'00' BLACK FAMILY CHANNEL                  
         DC    C'BFC '                                                          
         DC    C'FXC',X'013D',X'00',X'00' FXX                                   
         DC    C'FXX '                    WAS FSOC                              
         DC    C'INS',X'013E',X'00',X'00' THE INSPIRATIONAL NETWORK             
         DC    C'INSP'                                                          
         DC    C'AZE',X'013F',X'00',X'00' AZN TELEVISION                        
         DC    C'AZNT'                                                          
         DC    C'ESH',X'0140',X'00',X'00' ESPN HD                               
         DC    C'E1HD'                    NCC CODE CHANGE JAN23/15              
         DC    C'KJZ',X'0141',X'00',X'00' KJZZ                                  
         DC    C'KJZZ'                                                          
         DC    C'ALN',X'0142',X'00',X'00' AMERICAN LIFE NET (REPL GTV)          
         DC    C'ALN '                                                          
         DC    C'RSN',X'0143',X'00',X'00' ROYALS SPORTS NETWORK                 
         DC    C'RSTN'                                                          
         DC    C'AOT',X'0144',X'00',X'00' AMERICA ONE TV                        
         DC    C'AOT '                                                          
         DC    C'WRS',X'0145',X'00',X'00' THE WORSHIP NETWORK                   
         DC    C'WORS'                                                          
         DC    C'SPR',X'0146',X'00',X'00' THE SPORTSMAN                         
         DC    C'SPMN'                    WAS SPMC                              
         DC    C'URB',X'0147',X'00',X'00' URBAN TV                              
         DC    C'URB '                                                          
         DC    C'IN2',X'0148',X'00',X'00' TW/SAN ANTONIO'S LO CHANNEL           
         DC    C'IN26'                                                          
         DC    C'BH4',X'0149',X'00',X'00' CATCH 47 (BRIGHT HOUSE NET)           
         DC    C'ZC47'                                                          
         DC    C'AM2',X'014A',X'00',X'00' ATLANTA METRO CH. 26                  
         DC    C'AM26'                                                          
         DC    C'CH2',X'014B',X'00',X'00' COMCAST CHANNEL 22                    
         DC    C'CH22'                                                          
         DC    C'SPA',X'014C',X'00',X'00' SPANISH PADRES                        
         DC    C'SPD '                                                          
         DC    C'MAN',X'014D',X'00',X'00' MID ATLANIC SPORTS NETWORK            
         DC    C'MASN'                                                          
         DC    C'TV5',X'014E',X'00',X'00' LOCAL TROY CABLE CHANNEL              
         DC    C'TV52'                                                          
         DC    C'A23',X'014F',X'00',X'00' AKRON LO CH23                         
         DC    C'A23 '                                                          
         DC    C'NWB',X'0150',X'00',X'00' NEWS 12 BROOKLYN                      
         DC    C'NWBK'                                                          
         DC    C'NWH',X'0151',X'00',X'00' NEWS 12 HUDSON VALLEY                 
         DC    C'NWHV'                                                          
         DC    C'OTB',X'0152',X'00',X'00' OFF TRACK BETTING                     
         DC    C'OTB '                                                          
         DC    C'HBC',X'0153',X'00',X'00' HIAWATHA BROADBAND CHANNEL            
         DC    C'HBC '                                                          
         DC    C'ADT',X'0154',X'00',X'00' ADCAST TV                             
         DC    C'ADTV'                                                          
         DC    C'TDU',X'0155',X'00',X'00' TRIANGLE TV DUR                       
         DC    C'TDUR'                                                          
         DC    C'TFA',X'0156',X'00',X'00' TRIANGLE TV FAY                       
         DC    C'TFAY'                                                          
         DC    C'TRL',X'0157',X'00',X'00' TRIANGLE TV RAL                       
         DC    C'TRAL'                                                          
         DC    C'TWA',X'0158',X'00',X'00' TRIANGLE TV HENDERSON                 
         DC    C'TWAX'                                                          
         DC    C'NTV',X'0159',X'00',X'00' KTVB-BOISE                            
         DC    C'NTVB'                                                          
         DC    C'LOG',X'015A',X'00',X'00' LOGO                                  
         DC    C'LOGO'                                                          
         DC    C'VOD',X'015B',X'00',X'00' VIDEO DEMAND                          
         DC    C'VOD '                                                          
         DC    C'WLT',X'015C',X'00',X'00' A WEALTH OF ENTERTAINMENT             
         DC    C'AWE '                                                          
         DC    C'MVT',X'015D',X'00',X'00' MAV TV                                
         DC    C'MAV '                    WAS MVTV                              
         DC    C'SOR',X'015E',X'00',X'00' SORPRESA!                             
         DC    C'SOPR'                                                          
         DC    C'ILF',X'015F',X'00',X'00' HALOGEN                               
         DC    C'HALO'                    WAS ILFE WAS INLF                     
         DC    C'JSU',X'0160',X'00',X'00' JSU-TV 23                             
         DC    C'JSU '                                                          
         DC    C'CBL',X'0161',X'00',X'00' CBL6-HUDSON VALLEY                    
         DC    C'CBL6'                                                          
         DC    C'CLS',X'0162',X'00',X'00' CLASSIFIED CHANNEL                    
         DC    C'CLSS'                                                          
         DC    C'FX2',X'0163',X'00',X'00' FOX SPORTS 2                          
         DC    C'FS2 '                                                          
         DC    C'SAB',X'0164',X'00',X'00' BUFFALO SABRES HOCKEY                 
         DC    C'SABR'                                                          
         DC    C'ESU',X'0165',X'00',X'00' ESPNU                                 
         DC    C'ESNU'                                                          
         DC    C'OH ',X'0166',X'00',X'00' OH!                                   
         DC    C'OH! '                                                          
         DC    C'LEA',X'0167',X'00',X'00' LEASED ACCESS LONGFORM                
         DC    C'LEAS'                                                          
         DC    C'TU5',X'0168',X'00',X'00' TULSA 51                              
         DC    C'TU71'                                                          
         DC    C'CH6',X'0169',X'00',X'00' CHANNEL 6                             
         DC    C'CH6 '                                                          
         DC    C'ACE',X'016A',X'00',X'00' LEASED ACCESS CHANNEL                 
         DC    C'ACE '                                                          
         DC    C'UP ',X'016B',X'00',X'00' UP                                    
         DC    C'UP  '                                                          
         DC    C'NPH',X'016C',X'00',X'00' COMCAST SPRTS PH    <WAS CSP>         
         DC    C'NSPH'                    NCC CODE CHANGE JAN27/18              
         DC    C'CM3',X'016D',X'00',X'00' CABLEMART 31                          
         DC    C'CM31'                                                          
         DC    C'WOW',X'016E',X'00',X'00' WIDE OPEN WEST                        
         DC    C'WOW '                                                          
         DC    C'AHC',X'016F',X'00',X'00' AMERICAN HERO CH                      
         DC    C'AHC '                    NCC CODE CHANGE JAN23/15              
         DC    C'MST',X'0170',X'00',X'00' MAIN STREET TV                        
         DC    C'MSTV'                                                          
         DC    C'SNY',X'0171',X'00',X'00' SPORTSNET NEW YORK                    
         DC    C'SNY '                                                          
         DC    C'IHS',X'0172',X'00',X'00' INDIANA HS ATHLETIC ASSOC.            
         DC    C'IHSA'                                                          
         DC    C'FSL',X'0173',X'00',X'00' FOX SPORTS NET MINNEAPOLIS            
         DC    C'FSMN'                                                          
         DC    C'STU',X'0174',X'00',X'00' TIME WARNER/MN COMMON ZONE,MN         
         DC    C'KBL '                    WAS STUF                              
         DC    C'W13',X'0175',X'00',X'00' TIME WARNER/TERRE HAUTE,IN            
         DC    C'TW13'                                                          
         DC    C'T02',X'0176',X'00',X'00' TIME WARNER/LINCOLN,NE                
         DC    C'TWC2'                                                          
         DC    C'T31',X'0177',X'00',X'00' TIME WARNER/MEMPHIS,TN                
         DC    C'TW31'                                                          
         DC    C'T04',X'0178',X'00',X'00' TIME WARNER/CEN. KANSAS C.,MO         
         DC    C'TWC4'                                                          
         DC    C'T95',X'0179',X'00',X'00' TIME WARNER/MILWAUKEE,WI              
         DC    C'TW95'                                                          
         DC    C'STO',X'017A',X'00',X'00' SPORTS TIME OHIO                      
         DC    C'STO '                                                          
         DC    C'T05',X'017B',X'00',X'00' TWC5                                  
         DC    C'TWC5'                                                          
         DC    C'T98',X'017C',X'00',X'00' TWC98                                 
         DC    C'TW98'                                                          
         DC    C'T26',X'017D',X'00',X'00' TWC26                                 
         DC    C'TW26'                                                          
         DC    C'CXB',X'017E',X'00',X'00' COX 8 SB                              
         DC    C'CXSB'                                                          
         DC    C'KCH',X'017F',X'00',X'00' CHARTER/SOUTHERN OREGON INTER         
         DC    C'KCHT'                                                          
         DC    C'L15',X'0180',X'00',X'00' TIME WARNER/EL PASO, TX               
         DC    C'LF15'                                                          
         DC    C'L16',X'0181',X'00',X'00' TIME WARNER/WACO-WACO, TX             
         DC    C'LF16'                                                          
         DC    C'L18',X'0182',X'00',X'00' TIME WARNER/BEAUMONT, TX              
         DC    C'LF18'                                                          
         DC    C'L77',X'0183',X'00',X'00' TIME WARNER/AUSTIN, TX                
         DC    C'LF77'                                                          
         DC    C'LFC',X'0184',X'00',X'00' TIME WARNER/CORPUS CHRISTI,TX         
         DC    C'LFCC'                                                          
         DC    C'LFW',X'0185',X'00',X'00' TIME WARNER/WICHITA FALLS, TX         
         DC    C'LFWF'                                                          
         DC    C'LO7',X'0186',X'00',X'00' TIME WRNER/TW-HOUSTON ZONE,TX         
         DC    C'LO74'                                                          
         DC    C'NHV',X'0187',X'00',X'00' NEBRASKA HOME VIEW                    
         DC    C'NHV '                                                          
         DC    C'S76',X'0188',X'00',X'00' TIME WRNER/STATEN IS.CABLE,NY         
         DC    C'SI76'                                                          
         DC    C'T10',X'0189',X'00',X'00' TIME WARNER/BERGEN, NJ                
         DC    C'TW10'                                                          
         DC    C'T15',X'018A',X'00',X'00' TW/URA-UNION-ANSON-RICH, NC           
         DC    C'TW15'                                                          
         DC    C'T09',X'018B',X'00',X'00' TW/CLT/GAS/URA/SHE                    
         DC    C'TWC9'                                                          
         DC    C'V53',X'018C',X'00',X'00' TW/WNS-FORSY-YADK CITY, NC            
         DC    C'VM53'                                                          
         DC    C'V69',X'018D',X'00',X'00' TW/GBO-GUILFORD-ROCK CITY, NC         
         DC    C'VM69'                                                          
         DC    C'NEH',X'018E',X'00',X'00' NEW ENGLAND CABLE NEWS HD             
         DC    C'NESH'                                                          
         DC    C'OCL',X'018F',X'00',X'00' ORANGE COUNTY CH16                    
         DC    C'OCLF'                                                          
         DC    C'SBL',X'0190',X'00',X'00' SOUTH BAY CH16                        
         DC    C'SBLF'                                                          
         DC    C'ET6',X'0191',X'00',X'00' EPLUS TV6                             
         DC    C'ETV6'                                                          
         DC    C'GO ',X'0192',X'00',X'00' GOL TV                                
         DC    C'GOL '                                                          
         DC    C'KNL',X'0193',X'00',X'00' KNLF                                  
         DC    C'KNLF'                                                          
         DC    C'KTL',X'0194',X'00',X'00' KTLA                                  
         DC    C'KTLA'                                                          
         DC    C'T24',X'0195',X'00',X'00' TR24                                  
         DC    C'TR24'                                                          
         DC    C'T19',X'0196',X'00',X'00' TW19                                  
         DC    C'TW19'                                                          
         DC    C'T23',X'0197',X'00',X'00' TW23                                  
         DC    C'TW23'                                                          
         DC    C'TW2',X'0198',X'00',X'00' TW24                                  
         DC    C'TW24'                                                          
         DC    C'WLF',X'0199',X'00',X'00' WLF6                                  
         DC    C'WLF6'                                                          
         DC    C'WUA',X'019A',X'00',X'00' WUAB                                  
         DC    C'WUAB'                                                          
         DC    C'FRN',X'019B',X'00',X'00' FOX REALITY NETWORK                   
         DC    C'REAL'                                                          
         DC    C'CAR',X'019C',X'00',X'00' CARDINALS BASEBALL                    
         DC    C'CARD'                                                          
         DC    C'CH0',X'019D',X'00',X'00' CINCINNATI LONG FORM                  
         DC    C'CH02'                                                          
         DC    C'CXL',X'019E',X'00',X'00'                                       
         DC    C'CXLO'                                                          
         DC    C'CXP',X'019F',X'00',X'00'                                       
         DC    C'CXPD'                                                          
         DC    C'IGN',X'01A0',X'00',X'00' NON-VIDEO ENABLED IGUIDE              
         DC    C'IGUN'                                                          
         DC    C'IGV',X'01A1',X'00',X'00' VIDEO ENABLED IGUIDE                  
         DC    C'IGUV'                                                          
         DC    C'LWS',X'01A2',X'00',X'00' LOCAL WEATHER CHANNEL                 
         DC    C'LWS '                                                          
         DC    C'AFR',X'01A3',X'00',X'00' AFRICA CHANNEL                        
         DC    C'AFR '                                                          
         DC    C'BN1',X'01A4',X'00',X'00' BURDICK NEWS 10                       
         DC    C'BN10'                                                          
         DC    C'HM9',X'01A5',X'00',X'00' REAL ESTATE CH. 95                    
         DC    C'HM95'                                                          
         DC    C'TCH',X'01A6',X'00',X'00' TOURISM CONVENTION CHANNEL            
         DC    C'TCCH'                                                          
         DC    C'TW3',X'01A7',X'00',X'00'                                       
         DC    C'TW3 '                                                          
         DC    C'COA',X'01A8',X'00',X'00' COAST TV                              
         DC    C'COTV'                                                          
         DC    C'C71',X'01A9',X'00',X'00'                                       
         DC    C'CH71'                                                          
         DC    C'INF',X'01AA',X'00',X'00'                                       
         DC    C'INFO'                                                          
         DC    C'USD',X'01AB',X'00',X'00'                                       
         DC    C'UCSD'                                                          
         DC    C'C34',X'01AC',X'00',X'00' CENTRAL COAST CH.34                   
         DC    C'CC34'                                                          
         DC    C'MNW',X'01AD',X'00',X'00' MOUNTAIN WEST SPORTS NETWORK          
         DC    C'MTN '                                                          
         DC    C'VS ',X'01AE',X'00',X'00' NBC SPORTS NETWORK                    
         DC    C'NBCS'                    WAS VS                                
         DC    C'TGB',X'01AF',X'00',X'00' SPECTRUM NEWS GREENSBRO               
         DC    C'S1GO'                    <WAS ZSPG>                            
         DC    C'ALF',X'01B0',X'00',X'00' ARKANSAS LONG FORM                    
         DC    C'ARLF'                                                          
         DC    C'UMS',X'01B1',X'00',X'00' UNIMAS                                
         DC    C'UMAS'                                                          
         DC    C'CNV',X'01B2',X'00',X'00' COMCAST NETWORK - BURLINGTON          
         DC    C'CN8V'                                                          
         DC    C'CNY',X'01B3',X'00',X'00' COMCAST - ALBANY/SCHEN/TROY           
         DC    C'CN8Y'                                                          
         DC    C'MTE',X'01B4',X'00',X'00' METRO SPORTS - EL PASO                
         DC    C'MTRO'                                                          
         DC    C'TCL',X'01B5',X'00',X'00' SPECTRUM NEWS CHARLOTTE               
         DC    C'S1CT'                    <WAS ZSPC>                            
         DC    C'NRL',X'01B6',X'00',X'00' SPECTRUM NEWS RALEIGH                 
         DC    C'S1RL'                    <WAS ZSPR>                            
         DC    C'CX4',X'01B7',X'00',X'00' LOCAL COX MEDIA PROGRAMMING           
         DC    C'CXTV'                    WAS COX4                              
         DC    C'CWE',X'01B8',X'00',X'00' CW PLUS EAST SYSCODE 7530             
         DC    C'CWE '                                                          
         DC    C'GSH',X'01B9',X'00',X'00' COX MEDIA LOCAL REAL ESTATE           
         DC    C'GSH '                                                          
         DC    C'MHE',X'01BA',X'00',X'00' MIAMI HEAT ESPANOL                    
         DC    C'FESP'                                                          
         DC    C'E2H',X'01BB',X'00',X'00' ESPN 2 HD                             
         DC    C'ES2H'                                                          
         DC    C'FCP',X'01BC',X'00',X'00' FOX COLLEGE SPORTS-PACIFIC            
         DC    C'FCSP'                                                          
         DC    C'ITV',X'01BD',X'00',X'00' ITV (FORMERLY PAX TV)                 
         DC    C'ITV '                                                          
         DC    C'SUR',X'01BE',X'00',X'00' CANAL SUR                             
         DC    C'SUR '                                                          
         DC    C'TNH',X'01BF',X'00',X'00' TNT HD                                
         DC    C'TNTH'                                                          
         DC    C'TR3',X'01C0',X'00',X'00' MTV TR3                               
         DC    C'TR3S'                                                          
         DC    C'CET',X'01C1',X'00',X'00' COMCAST ENT TV5                       
         DC    C'CET5'                                                          
         DC    C'FCA',X'01C2',X'00',X'00' FOX COLLEGE SPORTS ATLANTIC           
         DC    C'FCSA'                                                          
         DC    C'FCC',X'01C3',X'00',X'00' FOX COLLEGE SPORTS CENTRAL            
         DC    C'FCSC'                                                          
         DC    C'TU7',X'01C4',X'00',X'00' TULSA 70                              
         DC    C'TU70'                                                          
         DC    C'OVL',X'01C5',X'00',X'00' OVERLAY                               
         DC    C'OVLY'                                                          
         DC    C'MCC',X'01C6',X'00',X'00' MEDIACOM CONNECTIONS CH.14            
         DC    C'MCC '                                                          
         DC    C'SCR',X'01C7',X'00',X'00' SCANNER CHANNEL                       
         DC    C'SCAN'                                                          
         DC    C'FMT',X'01C8',X'00',X'00' NUVOTV     <PREV SIT>                 
         DC    C'FMTV'                    NCC CODE CHANGE  JAN29/16             
         DC    C'BOO',X'01C9',X'00',X'00' BOOMERANG                             
         DC    C'BOOM'                                                          
         DC    C'FI1',X'01CA',X'00',X'00' VERIZON LOCAL                         
         DC    C'FI1 '                                                          
         DC    C'BNW',X'01CB',X'00',X'00' NEWS 10 BINGHAMTON                    
         DC    C'BNWS'                                                          
         DC    C'CSB',X'01CC',X'00',X'00' COMCAST MIDATLANTIC BALTIMORE         
         DC    C'BSNM'                                                          
         DC    C'AV ',X'01CD',X'00',X'00' AUTOVIEW                              
         DC    C'AV  '                                                          
         DC    C'CST',X'01CE',X'00',X'00' CBS SPORTS NETWORK                    
         DC    C'CBSS'                    WAS CSTV/WAS CBST                     
         DC    C'KAV',X'01CF',X'00',X'00' KAVT                                  
         DC    C'KAVT'                                                          
         DC    C'KTV',X'01D0',X'00',X'00' KTVK                                  
         DC    C'KTVK'                                                          
         DC    C'IFC',X'01D1',X'00',X'00' INDEPENDENT FILM CHANNEL              
         DC    C'IFC '                                                          
         DC    C'WCG',X'01D2',X'00',X'00'                                       
         DC    C'WCGV'                                                          
         DC    C'WMY',X'01D3',X'00',X'00'                                       
         DC    C'WMYV'                                                          
         DC    C'WVT',X'01D4',X'00',X'00'                                       
         DC    C'WVTV'                                                          
         DC    C'WXL',X'01D5',X'00',X'00'                                       
         DC    C'WXLV'                                                          
         DC    C'C49',X'01D6',X'00',X'00'                                       
         DC    C'CS49'                                                          
         DC    C'FO1',X'01D7',X'00',X'00' FIOS1                                 
         DC    C'FIO1'                                                          
         DC    C'CRE',X'01D8',X'00',X'00' COX REAL ESTATE                       
         DC    C'CRE '                                                          
         DC    C'WEB',X'01D9',X'00',X'00' ONLINE MEDIA                          
         DC    C'WEB '                                                          
         DC    C'MA2',X'01DA',X'00',X'00' MID-ATLANTIC SPORTS NETWORK 2         
         DC    C'MAS2'                                                          
         DC    C'DPP',X'01DB',X'00',X'00' DYNAMIC PUBLISHING PLATFORM           
         DC    C'DPP '                                                          
         DC    C'SAN',X'01DC',X'00',X'00' SOUTHERN AZ NEWS NETWORK              
         DC    C'SANN'                                                          
         DC    C'GME',X'01DD',X'00',X'00'                                       
         DC    C'WGME'                                                          
         DC    C'NYO',X'01DE',X'00',X'00'                                       
         DC    C'WNYO'                                                          
         DC    C'NYS',X'01DF',X'00',X'00'                                       
         DC    C'WNYS'                                                          
         DC    C'UHF',X'01E0',X'00',X'00'                                       
         DC    C'WUHF'                                                          
         DC    C'UTV',X'01E1',X'00',X'00'                                       
         DC    C'WUTV'                                                          
         DC    C'LCO',X'01E2',X'00',X'00'                                       
         DC    C'LCOX'                                                          
         DC    C'X48',X'01E3',X'00',X'00'                                       
         DC    C'CX48'                                                          
         DC    C'ABB',X'01E4',X'00',X'00'                                       
         DC    C'KABB'                                                          
         DC    C'MYS',X'01E5',X'00',X'00'                                       
         DC    C'KMYS'                                                          
         DC    C'SYT',X'01E6',X'00',X'00'                                       
         DC    C'WSYT'                                                          
         DC    C'SU2',X'01E7',X'00',X'00' SOMOS LATINAMERICA                    
         DC    C'SU2 '                                                          
         DC    C'CWL',X'01E8',X'00',X'00' CRAWL                                 
         DC    C'CRWL'                                                          
         DC    C'BTN',X'01E9',X'00',X'00' BIG TEN NETWORK                       
         DC    C'BTN '                                                          
         DC    C'WGN',X'01EA',X'00',X'20'                                       
         DC    C'WGN '                                                          
         DC    C'NEI',X'01EB',X'00',X'00'                                       
         DC    C'NEIN'                                                          
         DC    C'NBO',X'01EC',X'00',X'00' NBC SPORTS BOSTON   <WAS CSE>         
         DC    C'NSBO'                    NCC CODE CHANGE JAN27/18              
         DC    C'WPA',X'01ED',X'00',X'00' WAPA AMERICA                          
         DC    C'WAPA'                                                          
         DC    C'NNW',X'01EE',X'00',X'00' NBC SPRTS NORTHWEST <WAS CS1>         
         DC    C'NSNN'                    NCC CODE CHANGE JAN27/18              
         DC    C'FBN',X'01EF',X'00',X'00'                                       
         DC    C'FBN '                                                          
         DC    C'WSX',X'01F0',X'00',X'00'                                       
         DC    C'WSYX'                                                          
         DC    C'WTT',X'01F1',X'00',X'00'                                       
         DC    C'WTTE'                                                          
         DC    C'GSA',X'01F2',X'00',X'00' GO SCOUT AUTO                         
         DC    C'GSA '                                                          
         DC    C'WTE',X'01F3',X'00',X'00' NEWS 10 NOW - WATERTOWN               
         DC    C'ZN10'                                                          
         DC    C'CBH',X'01F4',X'00',X'00' COUNCIL BLUFFS HOME VIEW              
         DC    C'CBHV'                                                          
         DC    C'PXE',X'01F5',X'00',X'00' ION MEDIA EAST FEED                   
         DC    C'PXE '                                                          
         DC    C'PXP',X'01F6',X'00',X'00' ION MEDIA PACIFIC FEED                
         DC    C'PXP '                                                          
         DC    C'BBT',X'01F7',X'00',X'00' BLACK BELT TV                         
         DC    C'BBTV'                                                          
         DC    C'BRD',X'01F8',X'00',X'00' BRIDGE TV                             
         DC    C'BRDG'                                                          
         DC    C'CMN',X'01F9',X'00',X'00' CAMPUS MOVIE NETWORK                  
         DC    C'CMN '                                                          
         DC    C'GHI',X'01FA',X'00',X'00' GO SCOUTS HOME IOWA                   
         DC    C'GSHI'                                                          
         DC    C'GHN',X'01FB',X'00',X'00' GO SCOUTS HOME NEBRASKA               
         DC    C'GSHN'                                                          
         DC    C'WIS',X'01FC',X'00',X'00' GREEN BAY/APPLE SINCLAIR STN          
         DC    C'WISN'                                                          
         DC    C'IGM',X'01FD',X'00',X'00' INTERNET GAMING                       
         DC    C'IGAM'                                                          
         DC    C'GOZ',X'01FE',X'00',X'00' GO ARIZONA                            
         DC    C'GOAZ'                                                          
         DC    C'TRU',X'01FF',X'00',X'00' COURT TV                              
         DC    C'TRU '                                                          
         DC    C'SMS',X'0200',X'00',X'00' SHORT MESSAGE SERVICE                 
         DC    C'SMS '                                                          
         DC    C'CFE',X'0201',X'00',X'00' CENTRAL FL NEWS 13 EN ESPANOL         
         DC    C'CFNE'                                                          
         DC    C'LAT',X'0202',X'00',X'00'                                       
         DC    C'LATV'                                                          
         DC    C'CLL',X'0203',X'00',X'00' CLEVELAND LOCAL ACCESS                
         DC    C'CLLA'                                                          
         DC    C'TNV',X'0204',X'00',X'00' LA TELENOVELA                         
         DC    C'LTN '                    WAS TNVA                              
         DC    C'MLO',X'0205',X'00',X'00' MY LIFE ON DEMAND                     
         DC    C'MLOD'                                                          
         DC    C'UHD',X'0206',X'00',X'00' UNIVERSAL HD                          
         DC    C'UHD '                                                          
         DC    C'C74',X'0207',X'00',X'00' COX 74                                
         DC    C'CX74'                                                          
         DC    C'14W',X'0208',X'00',X'00' NEWS 14 WILMINGTON                    
         DC    C'Z14W'                                                          
         DC    C'TNS',X'0209',X'00',X'00'                                       
         DC    C'TENN'                                                          
         DC    C'WRD',X'020A',X'00',X'00' MYNETWORKTV/RETRO TV NETWORK          
         DC    C'WRDE'                                                          
         DC    C'FSH',X'020B',X'00',X'00' FOX SPORTS HOUSTON ALT                
         DC    C'FSHA'                                                          
         DC    C'S32',X'020C',X'00',X'00' SPORTS 32                             
         DC    C'SP32'                                                          
         DC    C'TVO',X'020D',X'00',X'00' TV OVERLAY                            
         DC    C'ITVO'                                                          
         DC    C'ZBH',X'020E',X'00',X'00' SPECTRUM SPORTS NETWORK               
         DC    C'ZSPS'                                                          
         DC    C'SLT',X'020F',X'00',X'00' CLOO                                  
         DC    C'CLOO'                    WAS SLTH                              
         DC    C'CMS',X'0210',X'00',X'00' CMC-TV                                
         DC    C'CMS '                                                          
         DC    C'NP+',X'0211',X'00',X'00' NBC SPORTS PH+      <WAS CNP>         
         DC    C'NSPP'                    NCC CODE CHANGE JAN27/18              
         DC    C'NW+',X'0212',X'00',X'00' NBC SPORTS WA+      <WAS CNM>         
         DC    C'NSWP'                    NCC CODE CHANGE JAN27/18              
         DC    C'MLB',X'0213',X'00',X'00' MLB NETWORK                           
         DC    C'MLBN'                                                          
         DC    C'NT+',X'0214',X'00',X'00' NEWS 12 PLUS                          
         DC    C'NWT+'                                                          
         DC    C'TNB',X'0215',X'00',X'00' SPECTRUM NEWS BUFFALO                 
         DC    C'S1BF'                    <WAS SPNB>                            
         DC    C'MC ',X'0216',X'00',X'00' MUSIC CHOICE                          
         DC    C'MC  '                                                          
         DC    C'DCT',X'0217',X'00',X'00' DEDICATED CHANNEL INTERACTIVE         
         DC    C'DCTV'                                                          
         DC    C'NHL',X'0218',X'00',X'00' NAT'L HOCKEY LEAGUE NETWORK           
         DC    C'NHLN'                                                          
         DC    C'PHO',X'0219',X'00',X'00' COMCAST - LOCAL ORIGINATION           
         DC    C'PHOT'                                                          
         DC    C'PRO',X'021A',X'00',X'00' PRODUCTION  NETWORK                   
         DC    C'PROD'                                                          
         DC    C'BIN',X'021B',X'00',X'00' BILL  INSERT NETWORK                  
         DC    C'BINS'                                                          
         DC    C'GIO',X'021C',X'00',X'00' GENERAL INFORMATION ON DEMAND         
         DC    C'GIOD'                                                          
         DC    C'HMM',X'021D',X'00',X'00' HALLMARK MV & MY                      
         DC    C'HMM '                    NCC CODE CHANGE JAN23/15              
         DC    C'AUO',X'021E',X'00',X'00' AUTO ON DEMAND                        
         DC    C'AUOD'                                                          
         DC    C'TRO',X'021F',X'00',X'00' TRAVEL ON DEMAND                      
         DC    C'TROD'                                                          
         DC    C'ETO',X'0220',X'00',X'00' ENTERTAINMENT ON DEMAND               
         DC    C'ETOD'                                                          
         DC    C'FHO',X'0221',X'00',X'00' FOX SPORTS NET - HOUSTON              
         DC    C'FSH '                                                          
         DC    C'CHI',X'0222',X'00',X'00' CHILLER                               
         DC    C'CHIL'                                                          
         DC    C'NBA',X'0223',X'00',X'00' NBA-TV                                
         DC    C'NBAT'                                                          
         DC    C'KAB',X'0224',X'00',X'00' KABC                                  
         DC    C'KABC'                                                          
         DC    C'KNB',X'0225',X'00',X'00' KNBC                                  
         DC    C'KNBC'                                                          
         DC    C'RLT',X'0226',X'00',X'00' RETIREMENT LIVING TELEVISION          
         DC    C'RLTV'                                                          
         DC    C'WCL',X'0227',X'00',X'00' WEDDING CENTRAL                       
         DC    C'WCL '                                                          
         DC    C'2EP',X'0228',X'00',X'00' ESP2 PAID PROGRAMMING                 
         DC    C'2EPD'                                                          
         DC    C'1O1',X'0229',X'00',X'00' TRAFFIC CHANNEL 101                   
         DC    C'T101'                                                          
         DC    C'NK2',X'022A',X'00',X'00' NICK TOO                              
         DC    C'NIKT'                                                          
         DC    C'YNN',X'022B',X'00',X'00' YOUR NEWS NOW                         
         DC    C'YNN '                                                          
         DC    C'BFO',X'022C',X'00',X'00' BUSINESS & FINANCE ON DEMAND          
         DC    C'BFOD'                                                          
         DC    C'EDO',X'022D',X'00',X'00' EDUCATION ON DEMAND                   
         DC    C'EDOD'                                                          
         DC    C'EMO',X'022E',X'00',X'00' EMPLOYMENT ON DEMAND                  
         DC    C'EMOD'                                                          
         DC    C'HGO',X'022F',X'00',X'00' HOME & GARDEN ON DEMAND               
         DC    C'HGOD'                                                          
         DC    C'PLO',X'0230',X'00',X'00' POLITICAL ON DEMAND                   
         DC    C'PLOD'                                                          
         DC    C'REO',X'0231',X'00',X'00' REAL ESTATE ON DEMAND                 
         DC    C'REOD'                                                          
         DC    C'RTO',X'0232',X'00',X'00' RETAIL ON DEMAND                      
         DC    C'RTOD'                                                          
         DC    C'SDP',X'0233',X'00',X'00' SPECTRUM DEPORTES                     
         DC    C'SPDP'                                                          
         DC    C'MGO',X'0234',X'00',X'00' MY GOVERNMENT ON DEMAND               
         DC    C'MGOD'                                                          
         DC    C'XBL',X'0235',X'00',X'00' XBOX LIVE                             
         DC    C'XBOX'                                                          
         DC    C'DPL',X'0236',X'00',X'00' DE PELICULA                           
         DC    C'DPLA'                                                          
         DC    C'HNE',X'0237',X'00',X'00' HD NET                                
         DC    C'HDNT'                    WAS HNET                              
         DC    C'SMI',X'0238',X'00',X'00' SMITHSONIAN HD                        
         DC    C'SMTD'                                                          
         DC    C'CBW',X'0239',X'00',X'00' CNBC WORLD                            
         DC    C'CNBW'                                                          
         DC    C'AJA',X'023A',X'00',X'00' AL JAZEERA AMERICA                    
         DC    C'AJAM'                                                          
         DC    C'SRC',X'023B',X'00',X'00' SEARCHLIGHT                           
         DC    C'SRCH'                                                          
         DC    C'OWN',X'023C',X'00',X'00' OPRAH WINFREY NETWORK                 
         DC    C'OWN '                                                          
         DC    C'CC ',X'023D',X'00',X'00' COOKING CHANNEL                       
         DC    C'CC  '                                                          
         DC    C'DFC',X'023E',X'00',X'00' DISC FAMILY                           
         DC    C'DFC '                    NCC CODE CHANGE JAN23/15              
         DC    C'TNA',X'023F',X'00',X'00' SPECTRUM NEWS ALBANY                  
         DC    C'S1AL'                    <WAS SPNA>                            
         DC    C'WTB',X'0240',X'00',X'00' NCC SAID WTBD WAS REPLACED            
         DC    C'WTBD'                      BY WNWS IN 2013                     
         DC    C'NGW',X'0241',X'00',X'00' NAT GEO WILD                          
         DC    C'NGWD'                                                          
         DC    C'TNR',X'0242',X'00',X'00' SPECTRUM NEWS ROCHESTR                
         DC    C'S1RC'                    <WAS SPNR>                            
         DC    C'NTS',X'0243',X'00',X'00' SPECTRUM NEWS SYRACUSE                
         DC    C'S1SY'                    <WAS SPNS>                            
         DC    C'NTA',X'0244',X'00',X'00' SPECTRUM NEWS AUSTIN                  
         DC    C'S1AU'                    <WAS SPNX>                            
         DC    C'MS2',X'0245',X'00',X'00' MADISON SQUARE GARDEN 2               
         DC    C'MSG2'                                                          
         DC    C'MP2',X'0246',X'00',X'00' MSG PLUS 2 FEED                       
         DC    C'MSP2'                                                          
         DC    C'CN2',X'0247',X'00',X'00' COMMONWEALTH NETWORK 2                
         DC    C'CN2 '                                                          
         DC    C'WBM',X'0248',X'00',X'00' COX MEDIA/WBMN OF MACON, GA           
         DC    C'WBMN'                                                          
         DC    C'FSK',X'0249',X'00',X'00' FOX SPORTS NET - OKLAHOMA             
         DC    C'FSOK'                                                          
         DC    C'RFI',X'024A',X'00',X'00' RFI VOD                               
         DC    C'RFIV'                                                          
         DC    C'IMA',X'024B',X'00',X'00' INFOMAS                               
         DC    C'INFO'                                                          
         DC    C'WFN',X'024C',X'00',X'00' WORLD FISHING NETWORK                 
         DC    C'WFNT'                    NCC CODE CHANGE JAN23/15              
         DC    C'RAM',X'024D',X'00',X'00' RAM SPORTS                            
         DC    C'RAMS'                                                          
         DC    C'TX1',X'024E',X'00',X'00' TEXAS CABLE NEWS                      
         DC    C'TXCN'                    WAS TXCH                              
         DC    C'OCP',X'024F',X'00',X'00' OC SPORTS PPV                         
         DC    C'OCPP'                                                          
         DC    C'SSH',X'0250',X'00',X'00' SPECTRUM SPORTS HAWAII  <OCS>         
         DC    C'SPSP'                    NCC CODE CHANGE JAN24/19              
         DC    C'RRR',X'0251',X'00',X'00' REMIND RECORD                         
         DC    C'RRR '                                                          
         DC    C'M23',X'0252',X'00',X'00' MIDCO SPORTSNET                       
         DC    C'MC23'                                                          
         DC    C'NW3',X'0253',X'00',X'00' NEWS PRESS 3 NOW                      
         DC    C'NEW3'                                                          
         DC    C'FS+',X'0254',X'00',X'00' FOX SPORTS PLUS                       
         DC    C'FSN+'                                                          
         DC    C'CI ',X'0255',X'00',X'00' CRIME & INVESTIGATION NETWORK         
         DC    C'CI  '                                                          
         DC    C'FPH',X'0256',X'00',X'00' FOX SPORTS NET PHILADELPHIA           
         DC    C'FSPH'                                                          
         DC    C'CCS',X'0257',X'00',X'00' COMCAST SPORTSNET CA A                
         DC    C'CCSF'                                                          
         DC    C'MHS',X'0258',X'00',X'00' MILITARY HISTORY CHANNEL              
         DC    C'MHIS'                                                          
         DC    C'DFM',X'0259',X'00',X'00' DISCOVERY FAMILIA                     
         DC    C'DFAM'                                                          
         DC    C'MMT',X'025A',X'00',X'00' MULTIMEDIOS TV                        
         DC    C'MMTV'                                                          
         DC    C'ASW',X'025B',X'00',X'00' ADULT SWIM                            
         DC    C'ADSM'                                                          
         DC    C'TCN',X'025C',X'00',X'00' THE COUNTRY CHANNEL                   
         DC    C'TCN '                                                          
         DC    C'HNY',X'025D',X'00',X'00' SPORTS NET NY - HARTFORD              
         DC    C'HSNY'                                                          
         DC    C'OVN',X'025E',X'00',X'00' ORLANDO VISITORS NETWORK              
         DC    C'OVN '                                                          
         DC    C'FXD',X'025F',X'00',X'00' FOX SPORTS NET-SAN DIEGO              
         DC    C'FSSD'                                                          
         DC    C'MT ',X'0260',X'00',X'00' MOTOR TREND         <WAS VEL>         
         DC    C'MT  '                    NCC CODE CHANGE JAN24/19              
         DC    C'VDT',X'0261',X'00',X'00' VOD TELESCOPING                       
         DC    C'VODT'                                                          
         DC    C'CHN',X'0262',X'00',X'00' COMCAST HOMETOWN NETWORK              
         DC    C'CHNI'                                                          
         DC    C'OMN',X'0263',X'00',X'00' ORBITS MEN                            
         DC    C'OMEN'                                                          
         DC    C'OWM',X'0264',X'00',X'00' ORBITS WOMEN                          
         DC    C'OWMN'                                                          
         DC    C'ELL',X'0265',X'00',X'00' CANAL ELLA                            
         DC    C'ELLA'                                                          
         DC    C'SSP',X'0266',X'00',X'00' SPECTRUM SPORTSNET                    
         DC    C'SPSN'                                                          
         DC    C'858',X'0267',X'00',X'00' TIME WARNER DEPORTES 858              
         DC    C'TDEP'                                                          
         DC    C'WVH',X'0268',X'00',X'00' WVVH                                  
         DC    C'WVVH'                                                          
         DC    C'LHN',X'0269',X'00',X'00' LONGHORN NETWORKS                     
         DC    C'LHN '                                                          
         DC    C'CL9',X'026A',X'00',X'00' COX LOCAL STATION 109                 
         DC    C'L109'                                                          
         DC    C'CL1',X'026B',X'00',X'00' COX LOCAL STATION 31                  
         DC    C'L31 '                                                          
         DC    C'DMN',X'026C',X'00',X'00' DIGITAL MEDIA AD NETWORK              
         DC    C'DMAN'                                                          
         DC    C'DWC',X'026D',X'00',X'00' TW DIGITAL WEATHER CRAWL              
         DC    C'SNIP'                                                          
         DC    C'PBA',X'026E',X'00',X'00' PAC-12 BAY AREA                       
         DC    C'P12B'                                                          
         DC    C'PMN',X'026F',X'00',X'00' PAC-12 MOUNTAIN                       
         DC    C'P12M'                                                          
         DC    C'POG',X'0270',X'00',X'00' PAC-12 OREGON                         
         DC    C'P12O'                                                          
         DC    C'PWT',X'0271',X'00',X'00' PAC-12 WASHINGTON                     
         DC    C'P12W'                                                          
         DC    C'PAZ',X'0272',X'00',X'00' PAC-12 ARIZONA                        
         DC    C'P12A'                                                          
         DC    C'PLA',X'0273',X'00',X'00' PAC-12 LOS ANGELES                    
         DC    C'P12L'                                                          
         DC    C'FDO',X'0274',X'00',X'00' FSN DETROIT +                         
         DC    C'FSDP'                    NCC CODE CHANGE JAN23/15              
         DC    C'CHP',X'0275',X'00',X'00' COMCAST HOMETOWN NETWORK PRE          
         DC    C'CHNS'                                                          
         DC    C'DXS',X'0276',X'00',X'00' DISNEY XD EN ESPANOL                  
         DC    C'DXDE'                                                          
         DC    C'FDS',X'0277',X'00',X'00' FOX DEPORTES SAN DIEGO                
         DC    C'FDSD'                                                          
         DC    C'NE1',X'0278',X'00',X'00' NESN PLUS                             
         DC    C'NES1'                                                          
         DC    C'ATS',X'0279',X'00',X'00' AT&T SPRTS SOUTHWEST<WAS RSW>         
         DC    C'ATSW'                    NCC CODE CHANGE JAN27/18              
         DC    C'KSB',X'027A',X'00',X'00' KSBY                                  
         DC    C'KSBY'                                                          
         DC    C'NSB',X'027B',X'00',X'00' NSBY                                  
         DC    C'NSBY'                                                          
         DC    C'TAL',X'027C',X'00',X'00' TV AUTO LOT                           
         DC    C'TVAL'                                                          
         DC    C'DMX',X'027D',X'00',X'00' DMAX+                                 
         DC    C'DMX+'                                                          
         DC    C'RFD',X'027E',X'00',X'00' RFD-TV                                
         DC    C'RFD '                                                          
         DC    C'CLX',X'027F',X'00',X'00' CENTURYLINK DMAX+                     
         DC    C'CLD+'                                                          
         DC    C'CTD',X'0280',X'00',X'00' CHARTER MEDIA DMAX+                   
         DC    C'CTD+'                                                          
         DC    C'CXD',X'0281',X'00',X'00' COX MEDIA DMAX+                       
         DC    C'CXD+'                                                          
         DC    C'VZD',X'0282',X'00',X'00' VERIZON DMAX+                         
         DC    C'VZD+'                                                          
         DC    C'YTV',X'0283',X'00',X'00' YOUTOO TV                             
         DC    C'YTOO'                                                          
         DC    C'NSX',X'0284',X'00',X'00' NBC SPORTS CA PLUS  <WAS CSX>         
         DC    C'NSCX'                    NCC CODE CHANGE JAN27/18              
         DC    C'DAM',X'0285',X'00',X'00' DESTINATION AMERICA                   
         DC    C'DAM '                                                          
         DC    C'AXS',X'0286',X'00',X'00' AXS TV                                
         DC    C'AXS '                                                          
         DC    C'UDN',X'0287',X'00',X'00' UNIVISION DEPORTES                    
         DC    C'TUDN'                    <WAS UDN>                             
         DC    C'ZNW',X'0288',X'00',X'00' CNWS WASH. DC                         
         DC    C'ZNWS'                         WAS <WNWS>                       
         DC    C'MSV',X'0289',X'00',X'00' MSG VARSITY                           
         DC    C'MSGV'                                                          
         DC    C'MTM',X'028A',X'00',X'00' MULTIMEDIOS                           
         DC    C'MLTM'                                                          
         DC    C'XBC',X'028B',X'00',X'00' XBOX CLEVELAND                        
         DC    C'XCLE'                                                          
         DC    C'XBM',X'028C',X'00',X'00' XBOX MEMPHIS                          
         DC    C'XMEM'                                                          
         DC    C'XBS',X'028D',X'00',X'00' XBOX ST. LOUIS                        
         DC    C'XSTL'                                                          
         DC    C'XBJ',X'028E',X'00',X'00' XBOX ST. JOSEPH                       
         DC    C'XSTJ'                                                          
         DC    C'FNP',X'028F',X'00',X'00' FOX SPORTS NORTH PLUS                 
         DC    C'FSNP'                                                          
         DC    C'BEI',X'0290',X'00',X'00' BEIN SPORTS NETWORK                   
         DC    C'BEIN'                                                          
         DC    C'OMV',X'0291',X'00',X'00' ORBITS DRAMA/MOVIES                   
         DC    C'OMOV'                                                          
         DC    C'OFM',X'0292',X'00',X'00' ORBITS FAMILY                         
         DC    C'OFAM'                                                          
         DC    C'OHM',X'0293',X'00',X'00' ORBITS HOME/LEISURE                   
         DC    C'OHOM'                                                          
         DC    C'ONW',X'0294',X'00',X'00' ORBITS NEWS/INFO                      
         DC    C'ONEW'                                                          
         DC    C'OMS',X'0295',X'00',X'00' ORBITS REALITY/MUSIC                  
         DC    C'OMUS'                                                          
         DC    C'OSP',X'0296',X'00',X'00' ORBITS SPORTS                         
         DC    C'OSPR'                                                          
         DC    C'OPC',X'0297',X'00',X'00' OPTIMUM CHANNEL                       
         DC    C'OPTC'                                                          
         DC    C'ADD',X'0298',X'00',X'00' ADDRESSABLE IMPRESSIONS               
         DC    C'ADDR'                                                          
         DC    C'GCI',X'0299',X'00',X'00' GCI 1 SPORTS                          
         DC    C'GCI1'                                                          
         DC    C'SLA',X'029A',X'00',X'00' SPORTS NETWORK - LA DODGERS           
         DC    C'SNLA'                                                          
         DC    C'TWB',X'029B',X'00',X'00' SPECTRUM NEWS ELMIRA                  
         DC    C'S1CN'                    <WAS SPNE>                            
         DC    C'TWH',X'029C',X'00',X'00' SPECTRUM NEWS HUDSON VALLEY           
         DC    C'S1HV'                    <WAS TWNH>                            
         DC    C'TWW',X'029D',X'00',X'00' TIME WARNER CBL WILMINGTON            
         DC    C'S1WL'                    <WAS SPNW>                            
         DC    C'TWK',X'029E',X'00',X'00' TIME WARNER CBL WACO/KILLEEN          
         DC    C'S1WC'                    <WAS TWNK>                            
         DC    C'FUN',X'029F',X'00',X'00' FUSION                                
         DC    C'FUSN'                                                          
         DC    C'NJR',X'02A0',X'00',X'00' NICK JR.                              
         DC    C'NKJR'                                                          
         DC    C'MHT',X'02A1',X'00',X'00' MY HOMETOWN TV                        
         DC    C'MHTV'                                                          
         DC    C'CTV',X'02A2',X'00',X'00' CARACOL TV                            
         DC    C'CRTV'                                                          
         DC    C'LDS',X'02A3',X'00',X'00' LA DODGERS SPANISH                    
         DC    C'SNLS'                                                          
         DC    C'WCN',X'02A4',X'00',X'00' SPECTRUM NEWS SAN ANTONIO             
         DC    C'S1SA'                    <WAS SPSA>                            
         DC    C'WCP',X'02A5',X'00',X'00' TIME WARNER CBL NEWS PALMDALE         
         DC    C'TWNP'                                                          
         DC    C'MDS',X'02A6',X'00',X'00' MIDCO SPORTS NETWORK                  
         DC    C'MDSN'                                                          
         DC    C'AT+',X'02A7',X'00',X'00' AT&T AUDIENCE +                       
         DC    C'ATA+'                                                          
         DC    C'AD+',X'02A8',X'00',X'00' AUDIENCE +                            
         DC    C'AUD+'                                                          
         DC    C'CL+',X'02A9',X'00',X'00' CENTURYLINK AUDIENCE +                
         DC    C'CLA+'                                                          
         DC    C'CM+',X'02AA',X'00',X'00' CHARTER MEDIA AUDIENCE +              
         DC    C'CTA+'                                                          
         DC    C'CX+',X'02AB',X'00',X'00' COX MEDIA AUDIENCE +                  
         DC    C'CXA+'                                                          
         DC    C'NGM',X'02AC',X'00',X'00' NATIONAL GEOGRAPHIC MUNDO             
         DC    C'NGM '                                                          
         DC    C'TW+',X'02AD',X'00',X'00' TIME WARNER AUDIENCE +                
         DC    C'TWA+'                                                          
         DC    C'VZ+',X'02AE',X'00',X'00' VERIZON AUDIENCE +                    
         DC    C'VZA+'                                                          
         DC    C'WBB',X'02AF',X'00',X'00' ONLINE BANNER SALES                   
         DC    C'WEBB'                                                          
         DC    C'WBV',X'02B0',X'00',X'00' ONLINE VIDEO SALES                    
         DC    C'WEBV'                                                          
         DC    C'APN',X'02B1',X'00',X'00' ASPIRE NETWORK                        
         DC    C'ASPR'                                                          
         DC    C'BEE',X'02B2',X'00',X'00' BEIN SPORTS ESPANOL                   
         DC    C'BEIE'                                                          
         DC    C'DAI',X'02B3',X'00',X'00' DYNAMIC AD INSERTION                  
         DC    C'DAI '                                                          
         DC    C'SEC',X'02B4',X'00',X'00' SEC NETWORK                           
         DC    C'SECN'                    NCC CODE CHANGE JAN23/15              
         DC    C'FW+',X'02B5',X'00',X'00' FOX SPORTS NET SOUTHWEST +            
         DC    C'FSSP'                                                          
         DC    C'SED',X'02B6',X'00',X'00' SPORTS ENHANCED DIGITAL NTWK          
         DC    C'SPEN'                                                          
         DC    C'RVL',X'02B7',X'00',X'00' REVOLT TV                             
         DC    C'RVLT'                                                          
         DC    C'STW',X'02B8',X'00',X'00' TIME WARNER SPORTS CAROLINAS          
         DC    C'TWSC'                                                          
         DC    C'P12',X'02B9',X'00',X'00' PAC-12 REGIONAL NETWORK               
         DC    C'P12N'                                                          
         DC    C'SND',X'02BA',X'00',X'00' SUNDANCE TV                           
         DC    C'SUND'                                                          
         DC    C'BLA',X'02BB',X'00',X'00' THE BLAZE TV                          
         DC    C'BLAZ'                                                          
         DC    C'VDP',X'02BC',X'00',X'00' VIDEO PLUS                            
         DC    C'VID+'                                                          
         DC    C'ENC',X'02BD',X'00',X'00' ENCORE HIT MOVIES                     
         DC    C'ENCR'                                                          
         DC    C'STR',X'02BE',X'00',X'00' STARZ                                 
         DC    C'STRZ'                                                          
         DC    C'RBT',X'02BF',X'00',X'00' ROADBLOCK TAKEOVER                    
         DC    C'RBTO'                                                          
         DC    C'ADP',X'02C0',X'00',X'00' AUDIENCE PLUS NETWORK                 
         DC    C'APNC'                                                          
         DC    C'PVT',X'02C1',X'00',X'00' PIVOT NETWORK                         
         DC    C'PIVT'                                                          
         DC    C'LAD',X'02C2',X'00',X'00' LINEAR ADDRESSABLE                    
         DC    C'LADR'                                                          
         DC    C'WGA',X'02C3',X'00',X'00' WGN AMERICA                           
         DC    C'WGNA'                                                          
         DC    C'MD2',X'02C4',X'00',X'00' MIDCO SPORTS NETWORK 2                
         DC    C'MDS2'                                                          
         DC    C'OTV',X'02C5',X'00',X'00' OUTSIDE TELEVISION                    
         DC    C'OSTV'                                                          
         DC    C'NKT',X'02C6',X'00',X'00' NICKTOONS                             
         DC    C'NKT '                                                          
         DC    C'SNA',X'02C7',X'00',X'00' SEC NETWORK ALTERNATE                 
         DC    C'SECO'                                                          
         DC    C'WNA',X'02C8',X'00',X'00' WEATHER NATION                        
         DC    C'WNAT'                                                          
         DC    C'CVA',X'02C9',X'00',X'00' CABLEVISION AUDIENCE +                
         DC    C'CVA+'                                                          
         DC    C'MCA',X'02CA',X'00',X'00' MIDCONTINENT AUDIENCE +               
         DC    C'MCA+'                                                          
         DC    C'SLK',X'02CB',X'00',X'00' SUDDENLINK AUDIENCE +                 
         DC    C'SLA+'                                                          
         DC    C'SRA',X'02CC',X'00',X'00' SPECTRUM REACH AUDIENCE +             
         DC    C'SRA+'                                                          
         DC    C'CDD',X'02CD',X'00',X'00' COMCAST DIGITAL ENT DISP ADV          
         DC    C'CDED'                                                          
         DC    C'CDV',X'02CE',X'00',X'00' COMCAST DIGITAL ENT VIDEO ADV         
         DC    C'CDEV'                                                          
         DC    C'SPS',X'02CF',X'00',X'00' SPONSORSHIP NETWORK                   
         DC    C'SPSR'                                                          
         DC    C'CVV',X'02D0',X'00',X'00' BERING - CABLEVISION VIDEO            
         DC    C'CVV+'                                                          
         DC    C'CLV',X'02D1',X'00',X'00' BERING - CENTURY LINK VIDEO           
         DC    C'CLV+'                                                          
         DC    C'CXV',X'02D2',X'00',X'00' BERING - COX VIDEO                    
         DC    C'CXV+'                                                          
         DC    C'MCV',X'02D3',X'00',X'00' BERING - MIDCONTINENT VIDEO           
         DC    C'MCV+'                                                          
         DC    C'SRV',X'02D4',X'00',X'00' BERING - SPECTRUM VIDEO               
         DC    C'SRV+'                                                          
         DC    C'SLV',X'02D5',X'00',X'00' BERING - SUDDENLINK VIDEO             
         DC    C'SLV+'                                                          
         DC    C'TWV',X'02D6',X'00',X'00' BERING - TIME WARNER VIDEO            
         DC    C'TWV+'                                                          
         DC    C'VZV',X'02D7',X'00',X'00' BERING - VERIZON VIDEO                
         DC    C'VZV+'                                                          
         DC    C'OCO',X'02D8',X'00',X'00' ORBITS COLLEGE                        
         DC    C'OCOL'                                                          
         DC    C'OLY',X'02D9',X'00',X'00' ORBITS OLYMPICS                       
         DC    C'OLYM'                                                          
         DC    C'ONA',X'02DA',X'00',X'00' ORBITS NASCAR                         
         DC    C'ONAS'                                                          
         DC    C'NB+',X'02DB',X'00',X'00' NBC SPRTS BAY AREA+ <WAS CBX>         
         DC    C'NSBX'                                                          
         DC    C'WBO',X'02DC',X'00',X'00' ONLINE OTTER MEDIA                    
         DC    C'WEBO'                                                          
         DC    C'O18',X'02DD',X'00',X'00' ORBITS NATIONAL A18+                  
         DC    C'ON18'                                                          
         DC    C'ONF',X'02DE',X'00',X'00' ORBITS NATIONAL FAMILIES              
         DC    C'ONFM'                                                          
         DC    C'ONM',X'02DF',X'00',X'00' ORBITS NATIONAL MEN                   
         DC    C'ONMN'                                                          
         DC    C'OWO',X'02E0',X'00',X'00' ORBITS NATIONAL WOMEN                 
         DC    C'ONWN'                                                          
         DC    C'NGF',X'02E1',X'00',X'00' NETWORK CLUSTER TARGETING             
         DC    C'NETC'                                                          
         DC    C'GGF',X'02E2',X'00',X'00' GENRE TARGETING                       
         DC    C'GENR'                                                          
         DC    C'PGF',X'02E3',X'00',X'00' PROGRAM/SHOW TARGETING                
         DC    C'PROG'                                                          
         DC    C'SGF',X'02E4',X'00',X'00' SPORTS GAME SHARE OF VOICE            
         DC    C'SPOR'                                                          
         DC    C'AGF',X'02E5',X'00',X'00' AUDIENCE EXTENSION TARGETING          
         DC    C'AUDX'                                                          
         DC    C'RGF',X'02E6',X'00',X'00' RUN OF NETWORK                        
         DC    C'RON '                                                          
         DC    C'CNT',X'02E7',X'00',X'00' CURATED NETWORK TARGETING             
         DC    C'CNET'                                                          
         DC    C'EGT',X'02E8',X'00',X'00' ENTERTAINMENT GENRE TARGETING         
         DC    C'ENGT'                                                          
         DC    C'SGT',X'02E9',X'00',X'00' GAME & SPORT GENRE TARGETING          
         DC    C'GSGT'                                                          
         DC    C'IGT',X'02EA',X'00',X'00' NEWS & INFO GENRE TARGETING           
         DC    C'NFOT'                                                          
         DC    C'CIV',X'02EB',X'00',X'00' COMCAST VIDEO                         
         DC    C'CIPV'                                                          
         DC    C'CIB',X'02EC',X'00',X'00' COMCAST DISPLAY                       
         DC    C'CIPB'                                                          
         DC    C'AIV',X'02ED',X'00',X'00' AT&T VIDEO                            
         DC    C'AIPV'                                                          
         DC    C'AIB',X'02EE',X'00',X'00' AT&T DISPLAY                          
         DC    C'AIPB'                                                          
         DC    C'CIP',X'02EF',X'00',X'00' COMCAST DISPLAY                       
         DC    C'CIPD'                                                          
         DC    C'AIP',X'02F0',X'00',X'00' AT&T DISPLAY                          
         DC    C'AIPD'                                                          
         DC    C'OML',X'02F1',X'00',X'00' ORBITS MET LIFE                       
         DC    C'ONMT'                                                          
         DC    C'I24',X'02F2',X'00',X'00' ISRAEL INTRNTNL 24-HOUR NEWS          
         DC    C'I24 '                                                          
         DC    C'XFL',X'02F3',X'00',X'00' XFINITY LATINO TV                     
         DC    C'XFLT'                                                          
         DC    C'IPD',X'02F4',X'00',X'00' IP/VOD DAI COMBO                      
         DC    C'IPOD'                                                          
         DC    C'BC2',X'02F5',X'00',X'00' BUCKEYE CABLE SPORTS NETWORK+         
         DC    C'BCS2'                                                          
         DC    C'TVE',X'02F6',X'00',X'00' TV EVERYWHERE                         
         DC    C'TVE '                                                          
         DC    C'BTO',X'02F7',X'00',X'00' BIG TEN NETWORK OVERFLOW              
         DC    C'BTNO'                                                          
         DC    C'CT3',X'02F8',X'00',X'00' WORCESTER CHARTER TV3 NEWS            
         DC    C'CTV3'                                                          
         DC    C'WA+',X'02F9',X'00',X'00' WOW DISPLAY AUDIENCE +                
         DC    C'WWA+'                                                          
         DC    C'WV+',X'02FA',X'00',X'00' WOW VIDEO AUDIENCE +                  
         DC    C'WWV+'                                                          
         DC    C'OND',X'02FB',X'00',X'00' ONMEDIA DISPLAY                       
         DC    C'OIPD'                                                          
         DC    C'ONV',X'02FC',X'00',X'00' ONMEDIA VIDEO                         
         DC    C'OIPV'                                                          
         DC    C'ATL',X'02FD',X'00',X'00' AT&T SPORTS LAS VEGAS                 
         DC    C'ATLV'                                                          
         DC    C'NSW',X'02FE',X'00',X'00' NBC SPORTS WASHINGTON PI              
         DC    C'NSWX'                                                          
         DC    C'ATU',X'02FF',X'00',X'00' AT&T SPORTSNET UTAH                   
         DC    C'ATUT'                                                          
         DC    C'AS2',X'0300',X'00',X'00' AT&T SPORTSNET SOUTHWEST2             
         DC    C'ATS2'                                                          
         DC    C'FVD',X'0301',X'00',X'00' FRONTIERVISION DISPLAY                
         DC    C'FIPD'                                                          
         DC    C'FVV',X'0302',X'00',X'00' FRONTIERVISION VIDEO                  
         DC    C'FIPV'                                                          
         DC    C'A+ ',X'0303',X'00',X'00' NCC A+                                
         DC    C'A+  '                                                          
         DC    C'EDG',X'0304',X'00',X'00' NCC EDGE                              
         DC    C'EDGE'                                                          
         DC    C'GON',X'0305',X'00',X'00' NCC GO                                
         DC    C'GO  '                                                          
         DC    C'XVO',X'0306',X'00',X'00' SPECTRUM SPORTS XCAST                 
         DC    C'XVO1'                                                          
         DC    C'SRF',X'0307',X'00',X'00' SPECTRUM SURF                         
         DC    C'SURF'                                                          
         DC    C'BT2',X'0308',X'00',X'00' BIG TEN NETWORK 2                     
         DC    C'BTN2'                                                          
         DC    C'CHD',X'0309',X'00',X'00' CHEDDAR U NETWORK                     
         DC    C'CHDU'                                                          
         DC    C'MSS',X'030A',X'00',X'00' MAXXSOUTH SPORTS NETWORK              
         DC    C'MSS '                                                          
         DC    C'RED',X'030B',X'00',X'00' CINCINNATI REDS                       
         DC    C'REDS'                                                          
         DC    C'PGD',X'030C',X'00',X'00' PROGRAMMATIC DISPLAY                  
         DC    C'PGDI'                                                          
         DC    C'PGV',X'030D',X'00',X'00' PROGRAMMATIC VIDEO                    
         DC    C'PGVI'                                                          
         DC    C'FX3',X'030E',X'00',X'00' FX MOVIE CHANNEL                      
         DC    C'FXM '                                                          
         DC    C'SC2',X'030F',X'00',X'00' SPECTRUM NEWS COLUMBUS                
         DC    C'S1CM'                    <WAS SPNC>                            
         DC    C'SPG',X'0310',X'00',X'00' SPECTRUM NEWS LEXINGTON               
         DC    C'S1LX'                    <WAS SPNG>                            
         DC    C'SPI',X'0311',X'00',X'00' SPECTRUM NEWS CINCINNATI              
         DC    C'S1CI'                    <WAS SPNI>                            
         DC    C'SPL',X'0312',X'00',X'00' SPECTRUM NEWS LOUISVILLE              
         DC    C'S1LV'                    <WAS SPNL>                            
         DC    C'SO2',X'0313',X'00',X'00' SPECTRUM NEWS COVINGTON               
         DC    C'S1CV'                    <WAS SPNO>                            
         DC    C'ST2',X'0314',X'00',X'00' SPECTRUM NEWS TOLEDO                  
         DC    C'S1TO'                    <WAS SPNT>                            
         DC    C'SPV',X'0315',X'00',X'00' SPECTRUM NEWS CLEVELAND               
         DC    C'S1CL'                    <WAS SPNV>                            
         DC    C'SPY',X'0316',X'00',X'00' SPECTRUM NEWS DAYTON                  
         DC    C'S1DY'                    <WAS SPNY>                            
         DC    C'SNL',X'0317',X'00',X'00' SPECTRUM NEWS LA                      
         DC    C'S1LA'                    <WAS SPLA>                            
         DC    C'SNK',X'0318',X'00',X'00' SPECTRUM NEWS MILWAUKEE               
         DC    C'S1MK'                    <WAS SPNK>                            
         DC    C'SGB',X'0319',X'00',X'00' SPECTRUM NEWS GREEN BAY               
         DC    C'S1GB'                    <WAS SPGB>                            
         DC    C'SMD',X'031A',X'00',X'00' SPECTRUM NEWS MADISON                 
         DC    C'S1MD'                    <WAS SPND>                            
         DC    C'CHC',X'031B',X'00',X'00' CHEDDARU - COMCAST                    
         DC    C'CHCO'                                                          
         DC    C'CHX',X'031C',X'00',X'00' CHEDDARU - COX MEDIA                  
         DC    C'CHCX'                                                          
         DC    C'CHS',X'031D',X'00',X'00' CHEDDARU - SPECTRUM                   
         DC    C'CHSP'                                                          
         DC    C'SNB',X'031E',X'00',X'00' SPECTRUM NEWS BOWLING GREEN           
         DC    C'S1BG'                    WAS <SPBG>                            
         DC    C'VDA',X'031F',X'00',X'00' VOD ADDRESSABLE                       
         DC    C'VODA'                                                          
         DC    C'DPS',X'0320',X'00',X'00' NCC DSP SUBSCRIBER TARGETING          
         DC    C'DSPS'                    VIDEO                                 
         DC    C'AUD',X'0321',X'00',X'00' AUDIENCE ONE                          
         DC    C'AUDO'                                                          
         DC    C'FMP',X'0322',X'00',X'00' FOX SPORTSNET MIDWEST+                
         DC    C'FSMP'                                                          
         DC    C'YVV',X'0323',X'00',X'00' YURVIEWVA                             
         DC    C'YVVA'                                                          
         DC    C'ACC',X'0324',X'00',X'00' ACC NETWORK                           
         DC    C'ACCN'                                                          
         DC    C'12A',X'0325',X'00',X'00' N12 ATLANTIC CITY                     
         DC    C'12AC'                                                          
         DC    C'BNC',X'0326',X'00',X'00' BLACK NEWS CHANNEL                    
         DC    C'BNC '                                                          
         DC    C'SNW',X'0327',X'00',X'00' SPECTRA NEWS 1 WORCESTER              
         DC    C'S1MA'                                                          
         DC    C'BBC',X'0328',X'00',X'00' BBC WORLD NEWS                        
         DC    C'BBCN'                                                          
         DC    C'MAR',X'0329',X'00',X'00' MARQUEE SPORTS NETWORK                
         DC    C'MARQ'                                                          
         DC    C'TGN',X'032A',X'00',X'00' TV GAMES NETWORK                      
         DC    C'TVG '                                                          
         DC    C'HDR',X'032B',X'00',X'00' HALLMARK DRAMA                        
         DC    C'HDRM'                                                          
         DC    C'N2L',X'032C',X'00',X'00' N12 ON AIR LOGO                       
         DC    C'N2LG'                                                          
         DC    C'AZM',X'032D',X'00',X'00' AZ MUNDOO                             
         DC    C'AZM '                                                          
         DC    C'AZC',X'032E',X'00',X'00' AZ CORAZON                            
         DC    C'AZC '                                                          
         DC    C'OAN',X'032F',X'00',X'00' ONE AMERICA NEWS                      
         DC    C'OAN '                                                          
         DC    C'TEN',X'0330',X'00',X'00' TEEN NICK                             
         DC    C'TEEN'                                                          
         DC    C'WCM',X'0331',X'00',X'00' WEATHER CHANNEL MANCELONA             
         DC    C'WCMA'                                                          
         DC    C'WPE',X'0332',X'00',X'00' WEATHER CHANNEL PETOSKEY              
         DC    C'WCPE'                                                          
         DC    C'WCI',X'0333',X'00',X'00' WEATHER CHANNEL IGNACE                
         DC    C'WCSI'                                                          
         DC    C'WCS',X'0334',X'00',X'00' WEATHER CHANNEL SAULT STE MAR         
         DC    C'WCSS'                                                          
         DC    C'WCC',X'0335',X'00',X'00' WEATHER CHANNEL CADILLAC              
         DC    C'WCCA'                                                          
         DC    C'WLU',X'0336',X'00',X'00' WEATHER CHANNEL LUDINGTON             
         DC    C'WCLU'                                                          
         DC    C'WCQ',X'0337',X'00',X'00' WEATHER CHANNEL MANISTQUE             
         DC    C'WCMQ'                                                          
         DC    C'HHG',X'0338',X'00',X'00' HOGAR DE HGTV                         
         DC    C'HHG '                                                          
         DC    C'SBT',X'0339',X'00',X'00' SPECTRUM NEWS BEAUMONT                
         DC    C'S1BT'                                                          
         DC    C'S1C',X'033A',X'00',X'00' SPECTRUM NEWS CORPUS CHRISTI          
         DC    C'S1CP'                                                          
         DC    C'SDF',X'033B',X'00',X'00' SPECTRUM NEWS DALLAS                  
         DC    C'S1DF'                                                          
         DC    C'SEP',X'033C',X'00',X'00' SPECTRUM NEWS ELPASO                  
         DC    C'S1EP'                                                          
         DC    C'SHR',X'033D',X'00',X'00' SPECTRUM NEWS HARLINGEN               
         DC    C'S1HR'                                                          
         DC    C'SHN',X'033E',X'00',X'00' SPECTRUM NEWS HOUSTON/SPRING          
         DC    C'S1HN'                                                          
         DC    C'SWF',X'033F',X'00',X'00' SPECTRUM NEWS WICHITA FALLS           
         DC    C'S1WF'                                                          
         DC    C'SLD',X'0340',X'00',X'00' SPECTRUM NEWS LAREDO                  
         DC    C'S1LD'                                                          
         DC    C'SMT',X'0341',X'00',X'00' SMITHSONIAN                           
         DC    C'SMTH'                                                          
         DC    C'SAV',X'0342',X'00',X'00' SPECTRUM NEWS ASHEVILLE               
         DC    C'S1AV'                                                          
         DC    C'NWN',X'0343',X'00',X'00' NEWS 12-NJ NORTH                      
         DC    C'NWNN'                                                          
         DC    C'NWS',X'0344',X'00',X'00' NEWS 12-NJ SOUTH                      
         DC    C'NWNS'                                                          
         DC    C'ZNY',X'0345',X'00',X'00' NEWS 12 PLUS NY                       
         DC    C'ZNPB'                                                          
         DC    C'ZNJ',X'0346',X'00',X'00' NEWS 12 PLUS NJ                       
         DC    C'ZNPN'                                                          
         DC    C'ZWC',X'0347',X'00',X'00' NEWS 12 PLUS WC                       
         DC    C'ZNPW'                                                          
         DC    C'ZCT',X'0348',X'00',X'00' NEWS 12 PLUS CT                       
         DC    C'ZNPC'                                                          
         DC    C'ZLI',X'0349',X'00',X'00' NEWS 12 PLUS LONG ISLAND              
         DC    C'ZNP '                                                          
         DC    C'ACW',X'034A',X'00',X'00' ACCUWEATHER                           
         DC    C'ACCU'                                                          
***********************************************************************         
* BRAND NEW CABLE NETWORKS GO ABOVE THIS COMMENT BLOCK                          
***********************************************************************         
*                                                                               
******************************************                                      
* HERE ARE THE OLDER 3 CHARACTER DS NETWORK CODES - DUPLICATE BINARIES          
* BEGIN BLOCK !!!!!!!!!!!!!!                                                    
******************************************                                      
FAM      DC    C'FAM',X'001A',X'00',X'49' FAMILY NETWORK      <NOW FRF>         
         DC    C'FRFM'                    NCC CODE CHANGE JAN29/16              
* WARNING - IF THE PREV NTWK CODE WAS TOP 24, IT MUST REMAIN TOP 24             
*                                                                               
         DC    C'JAZ',X'002C',X'00',X'00' CENTRIC             <NOW BHE>         
         DC    C'BHER'                    NCC CODE CHANGE JAN27/18              
         DC    C'MET',X'004A',X'00',X'00' TIMEWARNR KC HD     <NOW TKC>         
         DC    C'TWKC'                                                          
         DC    C'SPD',X'004E',X'00',X'00' FOX SPORTS 1        <NOW FS1>         
         DC    C'FS1 '                                                          
         DC    C'FIT',X'0050',X'00',X'00' DISCOVERY LIFE      <NOW DLF>         
         DC    C'DLIF'                    NCC CODE CHANGE JAN23/15              
         DC    C'FSB',X'005D',X'00',X'00' COMCAST BAY AREA MN <NOW NBY>         
         DC    C'NSBA'                    NCC CODE CHANGE JAN27/18              
         DC    C'FSP',X'006E',X'00',X'00' ROOT SPRTS PITTSBURG<NOW APT>         
         DC    C'ATPT'                    NCC CODE CHANGE JAN27/18              
         DC    C'FSR',X'006F',X'00',X'00' ROOT SPORTS ROCKY MT<NOW ARM>         
         DC    C'ATRM'                    NCC CODE CHANGE JAN27/18              
         DC    C'SNE',X'0080',X'00',X'00' COMCAST SN MA       <NOW NWA>         
         DC    C'NSWA'                    NCC CODE CHANGE JAN27/18              
         DC    C'RIN',X'0099',X'00',X'00' RI NEWS CHANNEL     <NOW OSN>         
         DC    C'OSN '                    NCC CODE CHANGE FEB01/21              
         DC    C'TVG',X'009C',X'00',X'00' POP FULL SCREEN     <NOW POP>         
         DC    C'POP '                    NCC CODE CHANGE JAN23/15              
         DC    C'TSO',X'00A1',X'00',X'20' SPORTSOUTH          <NOW FSU>         
         DC    C'FSSE'                    NCC CODE CHANGE JAN29/16              
         DC    C'STY',X'00A7',X'00',X'00' ESQUIRE             <NOW ESQ>         
         DC    C'ESQ '                                                          
         DC    C'FXM',X'00B2',X'00',X'00' FOX MOVIES          <NOW FXR>         
         DC    C'FXMR'                    NCC CODE CHANGE FEB03/17              
         DC    C'MTW',X'00C5',X'00',X'00' NEWS 12 RING+       <NOW ZNP>         
         DC    C'ZNP+'                    NCC CODE CHANGE FEB01/21              
         DC    C'HRN',X'00D8',X'00',X'00' HORSE RACING NTWK   <NOW TG2>         
         DC    C'TVG2'                    NCC CODE CHANGE JAN29/16              
         DC    C'CXX',X'00E0',X'00',X'00' COX7                <NOW YVA>         
         DC    C'YVAZ'                    NCC CODE CHANGE JAN27/18              
         DC    C'MUN',X'00E5',X'00',X'00' MUN2 CABLE   <NOW UVO>                
         DC    C'UVSO'                    NCC CODE CHANGE JAN29/16              
         DC    C'BIO',X'00E9',X'00',X'00' A&E BIOGRAPHY       <NOW FYI>         
         DC    C'FYI '                    NCC CODE CHANGE JAN23/15              
         DC    C'HST',X'00F0',X'00',X'00' HISTORY2            <NOW VIC>         
         DC    C'VICE'                    NCC CODE CHANGE FEB03/17              
         DC    C'SPK',X'0107',X'00',X'00' SPIKE TV            <NOW PAR>         
         DC    C'PAR '                    NCC CODE CHANGE JAN27/18              
         DC    C'UTI',X'0127',X'00',X'00' FOX LIFE                              
         DC    C'FOXL'                                                          
         DC    C'CSC',X'012E',X'00',X'00' COMCAST SPORTS CH   <NOW NCH>         
         DC    C'NSCH'                    NCC CODE CHANGE JAN27/18              
         DC    C'CSW',X'013A',X'00',X'00' CSN CALIFORNIA HD   <NOW NSC>         
         DC    C'NSCA'                    NCC CODE CHANGE JAN27/18              
         DC    C'FUL',X'0163',X'00',X'00' FOX SPORTS 2                          
         DC    C'FS2 '                                                          
         DC    C'GMC',X'016B',X'00',X'00' UP                                    
         DC    C'UP  '                                                          
         DC    C'CSP',X'016C',X'00',X'00' COMCAST SPRTS PH    <NOW NPH>         
         DC    C'NSPH'                    NCC CODE CHANGE JAN27/18              
         DC    C'MIL',X'016F',X'00',X'00' AMERICAN HERO CH                      
         DC    C'AHC '                    NCC CODE CHANGE JAN23/15              
         DC    C'NGB',X'01AF',X'00',X'00' TIME WARNR CBL NEWS GREENSBRO         
         DC    C'ZGTW'                                                          
         DC    C'TFC',X'01B1',X'00',X'00' UNIMAS                                
         DC    C'UMAS'                                                          
         DC    C'C14',X'01B5',X'00',X'00' TWC CHARLOTTE                         
         DC    C'S1CT'                    <WAS ZSPC>                            
         DC    C'R14',X'01B6',X'00',X'00' TWC RALEIGH                           
         DC    C'ZRTW'                    NCC CODE CHANGE JAN23/15              
         DC    C'SIT',X'01C8',X'00',X'00' NUVOTV  <NOW FMT>                     
         DC    C'FMTV'                    NCC CODE CHANGE JAN29/16              
         DC    C'CSE',X'01EC',X'00',X'00' COMCAST SN NEW ENG  <NOW NBO>         
         DC    C'NSBO'                    NCC CODE CHANGE JAN27/18              
         DC    C'CS1',X'01EE',X'00',X'00' COMCAST SN NW       <NOW NNW>         
         DC    C'NSNN'                    NCC CODE CHANGE JAN27/18              
         DC    C'CNP',X'0211',X'00',X'00' COMCAST NETWORK PH  <NOW NP+>         
         DC    C'NSPP'                    NCC CODE CHANGE JAN27/18              
         DC    C'CNM',X'0212',X'00',X'00' COMCAST NETWORK DC  <NOW NW+>         
         DC    C'NSWP'                    NCC CODE CHANGE JAN27/18              
         DC    C'N12',X'0214',X'00',X'00' NEWS 12 RING        <NOW NT+>         
         DC    C'NWT+'                    NCC CODE CHANGE FEB01/21              
         DC    C'YNB',X'0215',X'00',X'00' TIME WARNR CABLE NEWS BUFFALO         
         DC    C'S1BF'                    <WAS SPNB>                            
         DC    C'HMH',X'021D',X'00',X'00' HALLMARK MV & MY                      
         DC    C'HMM '                    NCC CODE CHANGE JAN23/15              
         DC    C'TWD',X'0233',X'00',X'00' TIME WARNER DEPORTES<NOW SDP>         
         DC    C'SPDP'                    NCC CODE CHANGE FEB03/17              
         DC    C'CRN',X'023A',X'00',X'00' AL JAZEERA AMERICA                    
         DC    C'AJAM'                                                          
         DC    C'HUB',X'023E',X'00',X'00' DISC FAMILY                           
         DC    C'DFC '                    NCC CODE CHANGE JAN23/15              
         DC    C'YNA',X'023F',X'00',X'00' TIME WARNER CABLE NEWS ALBANY         
         DC    C'TWNA'                                                          
         DC    C'YNR',X'0242',X'00',X'00' TIME WARNR CBL NEWS ROCHESTR          
         DC    C'S1RC'                    <WAS SPNR>                            
         DC    C'YNS',X'0243',X'00',X'00' TIME WARNR CBL NEWS SYRACUSE          
         DC    C'S1SY'                    <WAS SPNS>                            
         DC    C'YNT',X'0244',X'00',X'00' TIME WARNER CABLE NEWS AUSTIN         
         DC    C'S1AU'                    <WAS SPNX>                            
         DC    C'OCS',X'0250',X'00',X'00' OC SPORTS           <NOW SSH>         
         DC    C'SPSP'                    NCC CODE CHANGE JAN25/19              
         DC    C'VEL',X'0260',X'00',X'00' VELOCITY HD         <NOW MT >         
         DC    C'MT  '                    NCC CODE CHANGE JAN25/19              
         DC    C'TWN',X'0266',X'00',X'00' TW SPORTSNET        <NOW SSP>         
         DC    C'SPSN'                    NCC CODE CHANGE FEB03/17              
*****    DC    C'CSH',X'0279',X'00',X'00' ROOT SPORTS SOUTHWEST                 
*****    DC    C'RTSW'                    NCC CODE CHANGE JAN23/15              
         DC    C'RSW',X'0279',X'00',X'00' ROOT SPRTS SOUTHWEST<NOW ATS>         
         DC    C'ATSW'                    NCC CODE CHANGE JAN27/18              
         DC    C'CSX',X'0284',X'00',X'00' CSN CA ALT HD       <NOW NSX>         
         DC    C'NSCX'                    NCC CODE CHANGE JAN27/18              
         DC    C'WNW',X'0288',X'00',X'00' WNWS                <NOW ZNW>         
         DC    C'ZNWS'                    NCC CODE CHANGE FEB03/17              
         DC    C'CBX',X'02DB',X'00',X'00' CSN BAY ALT HD      <NOW NB+>         
         DC    C'NSBX'                    NCC CODE CHANGE JAN27/18              
******************************************                                      
* END BLOCK !!!!!!!!!!!!!!!!!                                                   
* HERE ARE THE OLDER 3 CHARACTER DS NETWORK CODES - DUPLICATE BINARIES          
******************************************                                      
*                                                                               
***********************************************************************         
* NEXT ENTRY GIVES DSPL TO CABLETB2 FROM CABLETAB                               
* IT HAS TO BE ON A DOUBLEWORD BOUNDARY                                         
***********************************************************************         
         DC    X'FFFFFF',AL4(((CABLETB2-CABLETAB+7)/8)*8)                       
         DC    XL4'00'                                                          
*                                                                               
         DS    0D                                                               
CABLETB2 DS    (CABLETB2-CABLETAB)C                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080SPCBLLIST 01/29/21'                                      
         END                                                                    
