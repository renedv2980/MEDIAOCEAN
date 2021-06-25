*          DATA SET GAICE01    AT LEVEL 016 AS OF 08/22/00                      
*PHASE TB1301A                                                                  
         TITLE 'ICE CREAM. RANDOM EVENT TABLES.'                                
EVENTS   CSECT                                                                  
*                                                                               
*        FIRST BYTE CONTAINS NUMBER OF TEMPSTR RECORDS NEEDED TO SAVE           
*        ENTIRE TABLE PHASE. THEN 7 BYTE ID FOR VERIFICATION                    
*                                                                               
TEMPSTR  DC    AL1(((EVENTEND-EVENTS)/2560)+1),CL7'$EVENT$'                     
*                                                                               
*        INDEX POINTERS - 2 BYTE OFFSET, 6 BYTE EVENT TYPE                      
*                                                                               
INDEX    DC    AL2(WTHRTAB-*),C'WEATHR' WEATHER TABLE                           
         DC    AL2(OCCNTAB-*),C'OCCASN' SPECIAL OCCASSION TAB                   
         DC    AL2(PPROBTAB-*),C'PPROBL' PLAYERS PROBLEM TABLE                  
         DC    AL2(DPROBTAB-*),C'DPROBL' OVERALL PROBLEM TABLE                  
*                                                                               
         DC    8X'FF'                   END OF TABLE POINTERS                   
         SPACE 3                                                                
*        EVENT TABLES.                                                          
*                                                                               
*        HEADER -                                                               
*              2 BYTE END OFFSET, 2 BYTE ENTRY LENGTH, 2 BYTE TOTAL             
*              PROBABILITY FILLED IN AT FIRST CALL                              
*        ELEMENT -                                                              
*              2 BYTE PROBABILITY RELATIVE TO OTHER ENTRIES                     
*              2 BYTE MAXIMUM 'HIT' COUNT                                       
*              TABLE DATA OF VARIABLE LENGTH DEPENDING ON TABLE                 
         EJECT                                                                  
*        WEATHER TABLE :-                                                       
*                                                                               
*        TABLE DATA -                                                           
*           1. 1 BYTE PERCENTAGE FACTOR. DEFINES EFFECT OF WEATHER              
*              ON DAY'S DEMAND, I.E. 100 = UNAFFECTED.                          
*           2. 98 BYTE DESCRIPTION OF WEATHER.                                  
*           3. 9 BYTE SHORT DESCRIPTION OF WEATHER.                             
*           -  TOTAL LENGTH 112 BYTES                                           
*                                                                               
WTHRTAB  DC    AL2(WTHREND-*),AL2(112),AL2(0)                                   
*                                                                               
WTHR001  DC    AL2(0030),AL2(000),AL1(090)                                      
         DC    CL49'THE WEATHER IS NOT EXPECTED TO BE TOO HOT, BUT NO'          
         DC    CL49' RAIN IS FORECAST.'                                         
         DC    CL9'FINE     '                                                   
WTHR002  DC    AL2(0020),AL2(000),AL1(110)                                      
         DC    CL49'A GOOD DAY TODAY, THE WEATHER SHOULD BE VERY WARM'          
         DC    CL49', BUT THERE IS A SLIM CHANCE OF LIGHT SHOWERS.'             
         DC    CL9'WARM     '                                                   
WTHR003  DC    AL2(0005),AL2(000),AL1(200)                                      
         DC    CL49'IT''S GONNA BE A SCORCHER TODAY. DEFINATELY NO RAI'         
         DC    CL49'N EXPECTED.'                                                
         DC    CL9'HOT      '                                                   
WTHR004  DC    AL2(0015),AL2(000),AL1(055)                                      
         DC    CL49'IT''S PROBABLY GOING TO RAIN TODAY ----- BUT THEN '         
         DC    CL49'ISN''T IT ALWAYS '                                          
         DC    CL9'SHOWERY  '                                                   
WTHR005  DC    AL2(0005),AL2(000),AL1(025)                                      
         DC    CL49'IT WILL BE COLD AND MISERABLE FOR MOST OF THIS MO'          
         DC    CL49'RNING, CLEARING UP TOWARDS MID AFTERNOON.'                  
         DC    CL9'HORRIBLE '                                                   
WTHR006  DC    AL2(0100),AL2(000),AL1(100)                                      
         DC    CL49'ITS GOING TO BE A NORMAL SUMMER''S DAY, I.E. PROBA'         
         DC    CL49'BLY SUNNY'                                                  
         DC    CL9'OK       '                                                   
WTHR007  DC    AL2(0005),AL2(001),AL1(035)                                      
         DC    CL49'IT''S GOING TO SNOW !!'                                     
         DC    CL49'         '                                                  
         DC    CL9'SNOW'                                                        
WTHREND  EQU  *-1                                                               
         EJECT                                                                  
*        SPECIAL OCCASSION TABLE :-                                             
*                                                                               
*        TABLE DATA -                                                           
*           1. SIX 1 BYTE PERCENTAGE FACTORS. EACH DEFINES EFFECT OF            
*              SPECIAL OCCASION ON DAYS DEMAND AT EACH OF THE SIX               
*              LOCATIONS . 100 = UNAFFECTED.                                    
*           2. 93 BYTE DESCRIPTION OF OCCASION                                  
*           3. 15 BYTE SHORT DESCRIPTION OF OCCASION.                           
*           -  TOTAL LENGTH 118 BYTES                                           
*                                                                               
OCCNTAB  DC    AL2(OCCNEND-*),AL2(118),AL2(0)                                   
*                                                                               
OCCN001  DC    AL2(0900),AL2(000),AL1(100,100,100,100,100,100)                  
         DC    93X'FF'   THIS IS A DEFAULT NULL ENTRY INDICATING NO             
         DC    CL15' '   SPECIAL OCCASION TODAY.                                
OCCN002  DC    AL2(0020),AL2(001),AL1(110,256,150,120,200,150)                  
         DC    CL49'TODAY WILL BE THE FIRST TIME SINCE BEFORE TUDOR T'          
         DC    CL44'IMES THAT A POPE HAS VISITED ENGLAND.       '               
         DC    CL15'POPE''S VISIT '                                             
OCCN003  DC    AL2(0020),AL2(000),AL1(200,256,150,120,200,150)                  
         DC    CL49'TODAY WILL BE ANOTHER ROYAL BIRTHDAY WITH PARADES'          
         DC    CL44' AND ENORMOUS CROWDS.'                                      
         DC    CL15'ROYAL PARADE  '                                             
OCCN004  DC    AL2(0020),AL2(000),AL1(170,150,150,140,125,180)                  
         DC    CL49'TODAY HAS BEEN DECLARED A PUBLIC HOLIDAY.'                  
         DC    CL44' '                                                          
         DC    CL15'PUBLIC HOLIDAY '                                            
OCCN005  DC    AL2(0020),AL2(001),AL1(100,100,100,256,100,100)                  
         DC    CL49'HARROD''S ARE HOLDING THEIR ANNUAL SALE TODAY'              
         DC    CL44' '                                                          
         DC    CL15'HARROD''S SALE'                                             
OCCNEND  EQU   *-1                                                              
         EJECT                                                                  
*        PLAYERS PROBLEM TABLE :-                                               
*                                                                               
*        TABLE DATA -                                                           
*           1. SIX 1 BYTE PERCENTAGE FACTORS. EACH DEFINES EFFECT OF            
*              PROBLEM ON DAYS SALES AT EACH OF THE SIX LOCATIONS.              
*              100 = UNAFFECTED. MUST BE 100 OR LESS                            
*           2. 74 BYTE DESCRIPTION OF PROBLEM                                   
*           -  TOTAL LENGTH 84  BYTES                                           
*                                                                               
PPROBTAB DC    AL2(PPROBEND-*),AL2(84),AL2(0)                                   
*                                                                               
PPROB001 DC    AL2(0900),AL2(000),AL1(100,100,100,100,100,100)                  
         DC    74X'FF'   THIS IS A NULL ENTRY INDICATING NO PROBLEM             
PPROB002 DC    AL2(0020),AL2(001),AL1(000,100,100,100,100,100)                  
         DC    CL49'THE VAN AT TRAFALGAR SQUARE WAS DAMAGED BY HEAVIE'          
         DC    CL25'S FROM THE COMPETITION   '                                  
PPROB003 DC    AL2(0020),AL2(000),AL1(100,000,100,100,100,100)                  
         DC    CL49'THE VAN AT BUCKINGHAM PALACE HAD A FREEZER FAILUR'          
         DC    CL25'E.'                                                         
PPROB004 DC    AL2(0020),AL2(001),AL1(100,100,030,100,100,100)                  
         DC    CL49'ONE OF THE VAN DRIVERS WAS ARRESTED FOR SOME MINO'          
         DC    CL25'R INFRINGEMENT.'                                            
PPROB005 DC    AL2(0020),AL2(001),AL1(100,100,100,100,060,100)                  
         DC    CL49'THE VAN AT ST.PAUL''S CATHEDRAL CAUGHT FIRE IN THE'         
         DC    CL25' EARLY AFTERNOON'                                           
PPROBEND EQU   *-1                                                              
         EJECT                                                                  
*        GENERAL PROBLEM TABLE :-                                               
*                                                                               
*        TABLE DATA -                                                           
*           1. SIX 1 BYTE PERCENTAGE FACTORS. EACH DEFINES EFFECT OF            
*              PROBLEM ON DAYS DEMAND AT EACH OF THE SIX                        
*              LOCATIONS . 100 = UNAFFECTED. IF IT IS A BENEFIT, THE            
*              FIRST FACTOR MUST BE HIGHER THAN 100                             
*           2. 74 BYTE DESCRIPTION OF PROBLEM                                   
*           -  TOTAL LENGTH 84  BYTES                                           
*                                                                               
DPROBTAB DC    AL2(DPROBEND-*),AL2(84),AL2(0)                                   
*                                                                               
DPROB001 DC    AL2(0900),AL2(000),AL1(100,100,100,100,100,100)                  
         DC    74X'FF'   THIS IS A NULL ENTRY INDICATING NO PROBLEMS            
DPROB002 DC    AL2(0020),AL2(001),AL1(010,050,040,070,030,090)                  
         DC    CL49'IT RAINED SO HEAVILY TODAY THAT SEVERAL PARTS OF '          
         DC    CL25'LONDON WERE FLOODED.     '                                  
DPROB003 DC    AL2(0020),AL2(001),AL1(010,010,010,010,010,010)                  
         DC    CL49'THERE WAS A TRANSPORT STRIKE AND ALL LINES TO LON'          
         DC    CL25'DON WERE CLOSED.         '                                  
DPROB004 DC    AL2(0020),AL2(001),AL1(150,150,150,150,150,180)                  
         DC    CL49'THERE WAS AN UNEXPECTED HEATWAVE TODAY.'                    
         DC    CL25' '                                                          
DPROBEND EQU   *-1                                                              
*                                                                               
EVENTEND EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016GAICE01   08/22/00'                                      
         END                                                                    
