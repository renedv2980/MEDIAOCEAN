*          DATA SET INTEREPS   AT LEVEL 025 AS OF 05/05/03                      
         TITLE 'DATAMGR - INTEREP SPECIAL PROCESSING TABLE'                     
*PHASE INTEREPA                                                                 
INTEREP  CSECT                                                                  
*                                                                               
***********************************************************************         
*- MOD LOG                                                                      
*  -------                                                                      
*  02/06/90  PJS  ADDED POINT PERSON TO MASTER ID LIST                          
*                                                                               
*  JUN06/90 (MRR) --- ADD MARKET RECORD TO MASET ID LIST                        
*                                                                               
*  OCT01/90 (MRR) --- ADD INTEREP I8/I9 AS REPS                                 
*                                                                               
*  09JAN91  (EFJ) --- FIX MIKES FIX ABOVE (CHANGE I2 TO I9)                     
*                                                                               
*  DEC23/91 (MRR) --- ADD ENTRIES FOR X'89' KEY                                 
*                                                                               
*  JUN19/92 (BU ) --- ADD ENTRIES FOR X'1A' KEY                                 
*                                                                               
*  APR14/93 (BU ) --- ADD INTEREP RM AS REP                                     
*                                                                               
*  OCT18/93 (BU ) --- ADD NEW COMPANY D4 AS REP                                 
*                                                                               
*  JAN27/94 (BU ) --- ADD ENTRIES FOR X'3A' AND X'3B' KEYS                      
*                                                                               
*  OCT12/94 (BU ) --- ADD NEW COMPANY IF AS REP                                 
*                                                                               
*  MAR27/95 (BU ) --- ADD NEW COMPANY S1 AS REP                                 
*                                                                               
*  MAY31/95 (BU ) --- ADD NEW COMPANY CM AS REP                                 
*                                                                               
*  SEP07/95 (BU ) --- ADD KATZ SETUP:                                           
*                     SALESPERSON IS MASTER                                     
*                     DIVISION, TEAM, GROUP, SUBGROUP ARE SUBS                  
*                                                                               
*  SEP28/95 (BU ) --- MAKE DIV/TEAM A MASTER AGAIN FOR KATZ GROUP               
*                                                                               
*  DEC15/95 (BU ) --- ADD 'TERRITORY' RECORD AS A MASTER                        
*                                                                               
*  JAN05/96 (BU ) --- ADD NEW COMPANY S3 AS REP                                 
*                                                                               
*  FEB27/96 (BU ) --- ADD NEW COMPANY CN AS REP                                 
*                                                                               
*  MAR14/96 (BU ) --- ADD NEW KATZ COMPANIES                                    
*                                                                               
*  JUN17/96 (BU ) --- ADD NEW INTEREP COMPANY: AQ                               
*                                                                               
*  SEP05/96 (BU ) --- ADD NEW COMPANY K6 AS REP (KATZ=NOT)                      
*                                                                               
*  OCT11/96 (BU ) --- DELETE TO,CM,I1 FROM TABLES                               
*                     ALSO DELETED DI,HN, PREVIOUSLY DROPPED                    
*                                                                               
*  FEB26/97 (BU ) --- ADD NEW COMPANY CG AS REP (INTEREP)                       
*                                                                               
*  MAR07/97 (BU ) --- ADD NEW COMPANY RS AS REP (KRG)                           
*                                                                               
*  APR15/97 (BU ) --- ADD TEST SETUP:  MASTER/SUB1/SUB2(MS/U1/U2)               
*                                                                               
*  JUL21/97 (BU ) --- ADD TEST SETUP:  KTVNYT/KAMNYT/KCONYT/KNANYT              
*                                                                               
*  JAN06/98 (BU ) --- ADD TERRITORY PASSIVE                                     
*                                                                               
*  FEB02/98 (BU ) --- ADD TEST SETUP:  SELNYT/SCPNYT/SRPNYT                     
*                                                                               
*  MAR17/98 (BU ) --- ADD NEW COMPANY L7 AS REP (INTEREP)                       
*                     ADD INTEREP TEST IDS FOR S/P AS MASTER TESTING            
*                                                                               
*  APR22/98 (BU ) --- UPGRADE INTEREP TO S/P AT MASTER LEVEL                    
*                                                                               
*  MAY19/98 (BU ) --- ADD NEW COMPANY IB AS REP (INTEREP)                       
*                                                                               
*  MAR08/99 (BU ) --- ADD NEW PASSIVE BA FOR AGENCY2 RECORD                     
*                                                                               
*  MAR19/99 (BU ) --- ADD NEW COMPANY V0 AS REP (KATZ TV)                       
*                                                                               
*  MAR25/99 (BU ) --- ADD NEW COMPANY NX AS REP (INTEREP)                       
*                                                                               
*  APR12/99 (BU ) --- ADD NEW COMPANY UO AS REP (INTEREP)                       
*                                                                               
*  APR22/99 (BU ) --- ADD NEW PASSIVE 86 FOR S/P RECORD                         
*                                                                               
*  MAY03/99 (BU ) --- ADD NATL PUBLIC MASTER + RADIO COMPANY (NEED              
*                     LIVE CODES!!)                                             
*                     ADD NATL PUBLIC TEST CODES                                
*                     DELETE ALL DEACTIVATED CODES FROM TABLE                   
*                                                                               
*  AUG10/99 (BU ) --- ADD S/P 2NDARY KEY X'46'                                  
*                                                                               
*  AUG16/99 (BU ) --- ADD NEW COMPANY QD AS REP (KRG)                           
*                                                                               
*  DEC02/99 (BU ) --- ADD NEW COMPANY NU AS REP (KRG)                           
*                                                                               
*  FEB28/00 (BU ) --- MOVE TEAM TO MASTER LEVEL FOR KATZ TV                     
*                                                                               
*  APR24/00 (BU ) --- ADD NEW COMPANY G8 AS REP (KRG)                           
*                                                                               
*  MAR20/01 (BU ) --- ADD NEW COMPANY J0 AS REP (KRG)                           
*                                                                               
*  SEP21/01 (BU ) --- ADD NEW COMPANY WC AS REP (KRG)                           
*                                                                               
*  MAY10/02 (BU ) --- ADD NEW COMPANY 8K AS REP (KTV)                           
*                                                                               
*  NOV18/02 (BU ) --- ADD S/P AS MASTER FOR MASTER/SUB1/SUB2/SUB3               
*                                                                               
*  NOV21/02 (BU ) --- ADD 9A  AS MASTER KEY                                     
*                                                                               
*  MAY05/03 (BU ) --- MASTER/SUB SETUP:                                         
*                     6S    /6T   /6U   /6V                                     
*                     KRGNYT/KRNYT/KHNYT/CHRNYT                                 
*                                                                               
*                                                                               
*                     ***  END TOMBSTONE  ***                                   
***********************************************************************         
         SPACE                                                                  
*                                                                               
* IF YOU UPDATE THESE - BETTER LOOK AT DMDMGRREP AS WELL!!!                     
*                                                                               
RTABID   EQU   0                   X'MASTER KEY ID'                             
RTABDISP EQU   1                   REP CODE DISPLACEMENT IN KEY                 
RTABSUB  EQU   2                   SUBSIDIARY REP CODE                          
RTABMAST EQU   4                   MASTER REP CODE                              
RTABELEN EQU   6                   ENTRY LENGTH                                 
*                                                                               
*                                                                               
* SUBSIDIARY =  MCGAVREN  MASTER = INTEREP                                      
*                                                                               
REPMTBL  EQU   *                                                                
*                                                                               
         DC    X'05',AL1(23),C'MG',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'MG',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'MG',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'MG',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'MG',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'MG',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'MG',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'MG',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'MG',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'MG',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'MG',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'MG',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'MG',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'MG',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'MG',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'MG',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'MG',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'MG',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'MG',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'MG',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'MG',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'MG',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'MG',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'MG',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'MG',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  GROUP W   MASTER = INTEREP                                      
*                                                                               
*        DC    X'05',AL1(23),C'GP',C'IR'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'GP',C'IR'     SALESPERSON                        
*        DC    X'46',AL1(22),C'GP',C'IR'     SALESPERSON                        
*        DC    X'86',AL1(02),C'GP',C'IR'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'GP',C'IR'     GROUP                              
*        DC    X'08',AL1(25),C'GP',C'IR'     ADVERTISER                         
*        DC    X'88',AL1(25),C'GP',C'IR'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'GP',C'IR'     PRODUCT                            
*        DC    X'89',AL1(10),C'GP',C'IR'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'GP',C'IR'     AGENCY                             
*        DC    X'1A',AL1(25),C'GP',C'IR'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'GP',C'IR'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'GP',C'IR'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'GP',C'IR'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'GP',C'IR'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'GP',C'IR'     CLASS                              
*        DC    X'0F',AL1(23),C'GP',C'IR'     CATEGORY                           
*        DC    X'2A',AL1(22),C'GP',C'IR'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'GP',C'IR'     MARKET                             
*        DC    X'8B',AL1(16),C'GP',C'IR'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'GP',C'IR'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'GP',C'IR'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'GP',C'IR'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'GP',C'IR'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'GP',C'IR'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  NONREP    MASTER = INTEREP                                      
* (ADDED 12/22/89   PJS)                                                        
*                                                                               
         DC    X'05',AL1(23),C'I2',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'I2',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'I2',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'I2',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'I2',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'I2',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'I2',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'I2',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'I2',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'I2',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'I2',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'I2',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'I2',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'I2',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'I2',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'I2',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'I2',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'I2',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'I2',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'I2',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'I2',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'I2',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'I2',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'I2',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'I2',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  I8/CABALLERO RADIO MASTER = INTEREP                             
* (ADDED OCT01/90   MRR)                                                        
*                                                                               
         DC    X'05',AL1(23),C'I8',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'I8',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'I8',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'I8',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'I8',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'I8',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'I8',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'I8',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'I8',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'I8',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'I8',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'I8',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'I8',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'I8',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'I8',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'I8',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'I8',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'I8',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'I8',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'I8',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'I8',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'I8',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'I8',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'I8',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'I8',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  I9/INTEREP NEW 2    MASTER = INTEREP                            
* (ADDED OCT01/90   MRR)                                                        
*                                                                               
         DC    X'05',AL1(23),C'I9',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'I9',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'I9',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'I9',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'I9',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'I9',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'I9',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'I9',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'I9',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'I9',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'I9',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'I9',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'I9',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'I9',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'I9',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'I9',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'I9',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'I9',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'I9',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'I9',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'I9',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'I9',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'I9',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'I9',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'I9',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  RM/INTEREP NEW RADIO MARKETING  MASTER = INTEREP                
* (ADDED APR14/93   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'RM',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'RM',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'RM',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'RM',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'RM',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'RM',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'RM',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'RM',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'RM',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'RM',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'RM',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'RM',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'RM',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'RM',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'RM',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'RM',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'RM',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'RM',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'RM',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'RM',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'RM',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'RM',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'RM',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'RM',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'RM',C'IR'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  D4/INTEREP DI+HN MERGER COMPANY  MASTER = INTEREP               
* (ADDED OCT18/93   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'D4',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'D4',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'D4',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'D4',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'D4',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'D4',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'D4',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'D4',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'D4',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'D4',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'D4',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'D4',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'D4',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'D4',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'D4',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'D4',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'D4',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'D4',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'D4',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'D4',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'D4',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'D4',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'D4',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'D4',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'D4',C'IR'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  IF/INFINITY RADIO COMPANY  MASTER = INTEREP                     
* (ADDED OCT12/94   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'IF',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'IF',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'IF',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'IF',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'IF',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'IF',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'IF',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'IF',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'IF',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'IF',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'IF',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'IF',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'IF',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'IF',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'IF',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'IF',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'IF',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'IF',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'IF',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'IF',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'IF',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'IF',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'IF',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'IF',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'IF',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  S1/SHAMROCK RADIO COMPANY  MASTER = INTEREP                     
* (ADDED MAR27/95   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'S1',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'S1',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'S1',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'S1',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'S1',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'S1',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'S1',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'S1',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'S1',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'S1',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'S1',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'S1',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'S1',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'S1',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'S1',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'S1',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'S1',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'S1',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'S1',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'S1',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'S1',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'S1',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'S1',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'S1',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'S1',C'IR'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  CN/CLEAR CHANNEL RADIO     MASTER = INTEREP                     
* (ADDED FEB27/96   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'CN',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'CN',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'CN',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'CN',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'CN',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'CN',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'CN',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'CN',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'CN',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'CN',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'CN',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'CN',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'CN',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'CN',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'CN',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'CN',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'CN',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'CN',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'CN',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'CN',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'CN',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'CN',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'CN',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'CN',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'CN',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  AQ/ALLIED RADIO PARTNERS   MASTER = INTEREP                     
* (ADDED JUN17/96   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'AQ',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'AQ',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'AQ',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'AQ',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'AQ',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'AQ',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'AQ',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'AQ',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'AQ',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'AQ',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'AQ',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'AQ',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'AQ',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'AQ',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'AQ',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'AQ',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'AQ',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'AQ',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'AQ',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'AQ',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'AQ',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'AQ',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'AQ',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'AQ',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'AQ',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  CG/CBSTEST (INF+GP MERGER) MASTER = INTEREP                     
* (ADDED FEB26/97   BU )                                                        
*                                                                               
*        DC    X'05',AL1(23),C'CG',C'IR'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'CG',C'IR'     SALESPERSON                        
*        DC    X'46',AL1(22),C'CG',C'IR'     SALESPERSON                        
*        DC    X'86',AL1(02),C'CG',C'IR'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'CG',C'IR'     GROUP                              
*        DC    X'08',AL1(25),C'CG',C'IR'     ADVERTISER                         
*        DC    X'88',AL1(25),C'CG',C'IR'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'CG',C'IR'     PRODUCT                            
*        DC    X'89',AL1(10),C'CG',C'IR'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'CG',C'IR'     AGENCY                             
*        DC    X'1A',AL1(25),C'CG',C'IR'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'CG',C'IR'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'CG',C'IR'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'CG',C'IR'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'CG',C'IR'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'CG',C'IR'     CLASS                              
*        DC    X'0F',AL1(23),C'CG',C'IR'     CATEGORY                           
*        DC    X'2A',AL1(22),C'CG',C'IR'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'CG',C'IR'     MARKET                             
*        DC    X'8B',AL1(16),C'CG',C'IR'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'CG',C'IR'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'CG',C'IR'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'CG',C'IR'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'CG',C'IR'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'CG',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  L7/CABALLERO TV MASTER = INTEREP                                
* (ADDED MAR17/98   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'L7',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'L7',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'L7',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'L7',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'L7',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'L7',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'L7',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'L7',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'L7',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'L7',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'L7',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'L7',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'L7',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'L7',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'L7',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'L7',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'L7',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'L7',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'L7',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'L7',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'L7',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'L7',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'L7',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'L7',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'L7',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  IB/ABC RADIO MASTER = INTEREP                                   
* (ADDED MAY19/98   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'IB',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'IB',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'IB',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'IB',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'IB',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'IB',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'IB',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'IB',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'IB',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'IB',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'IB',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'IB',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'IB',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'IB',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'IB',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'IB',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'IB',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'IB',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'IB',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'IB',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'IB',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'IB',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'IB',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'IB',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'IB',C'IR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  NX/PUBLIC NETWORK RADIO  MASTER = INTEREP                       
* (ADDED MAR25/99   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'NX',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'NX',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'NX',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'NX',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'NX',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'NX',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'NX',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'NX',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'NX',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'NX',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'NX',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'NX',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'NX',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'NX',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'NX',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'NX',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'NX',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'NX',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'NX',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'NX',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'NX',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'NX',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'NX',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'NX',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'NX',C'IR'     TERRITORY CODE                     
**>UO>                                                                          
*                                                                               
* SUBSIDIARY =  UO/CUMULUS RADIO         MASTER = INTEREP                       
* (ADDED APR12/99   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'UO',C'IR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'UO',C'IR'     SALESPERSON                        
         DC    X'46',AL1(22),C'UO',C'IR'     SALESPERSON                        
         DC    X'86',AL1(02),C'UO',C'IR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'UO',C'IR'     GROUP                              
         DC    X'08',AL1(25),C'UO',C'IR'     ADVERTISER                         
         DC    X'88',AL1(25),C'UO',C'IR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'UO',C'IR'     PRODUCT                            
         DC    X'89',AL1(10),C'UO',C'IR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'UO',C'IR'     AGENCY                             
         DC    X'1A',AL1(25),C'UO',C'IR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'UO',C'IR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'UO',C'IR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'UO',C'IR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'UO',C'IR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'UO',C'IR'     CLASS                              
         DC    X'0F',AL1(23),C'UO',C'IR'     CATEGORY                           
         DC    X'2A',AL1(22),C'UO',C'IR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'UO',C'IR'     MARKET                             
         DC    X'8B',AL1(16),C'UO',C'IR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'UO',C'IR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'UO',C'IR'     POINT PERSON                       
         DC    X'3A',AL1(22),C'UO',C'IR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'UO',C'IR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'UO',C'IR'     TERRITORY CODE                     
**>UO>                                                                          
*                                                                               
*    KATZ FILE SETUP BEGINS AT THIS POINT                                       
*                                                                               
* SUBSIDIARY =  BF/BANNER RADIO            MASTER = KATZ RADIO GROUP            
* (ADDED SEP07/95   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'BF',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'BF',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'BF',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'BF',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'BF',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'BF',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'BF',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'BF',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'BF',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'BF',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'BF',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'BF',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'BF',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'BF',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'BF',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'BF',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'BF',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'BF',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'BF',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'BF',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'BF',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'BF',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'BF',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'BF',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'BF',C'K3'     TERRITORY CODE                     
*                                                                               
**QD>>                                                                          
*                                                                               
* SUBSIDIARY =  QD/SPECTRUM SALES          MASTER = KATZ RADIO GROUP            
* (ADDED AUG16/99   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'QD',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'QD',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'QD',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'QD',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'QD',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'QD',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'QD',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'QD',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'QD',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'QD',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'QD',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'QD',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'QD',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'QD',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'QD',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'QD',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'QD',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'QD',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'QD',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'QD',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'QD',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'QD',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'QD',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'QD',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'QD',C'K3'     TERRITORY CODE                     
*                                                                               
**QD>>                                                                          
**NU>>                                                                          
*                                                                               
* SUBSIDIARY =  NU/CLEAR CHANNEL RADIO     MASTER = KATZ RADIO GROUP            
* (ADDED DEC02/99   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'NU',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'NU',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'NU',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'NU',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'NU',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'NU',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'NU',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'NU',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'NU',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'NU',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'NU',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'NU',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'NU',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'NU',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'NU',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'NU',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'NU',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'NU',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'NU',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'NU',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'NU',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'NU',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'NU',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'NU',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'NU',C'K3'     TERRITORY CODE                     
*                                                                               
**NU>>                                                                          
*                                                                               
**G8>>                                                                          
*                                                                               
* SUBSIDIARY =  G8/KATZ INTERACTIVE MEDIA  MASTER = KATZ RADIO GROUP            
* (ADDED APR24/00   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'G8',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'G8',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'G8',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'G8',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'G8',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'G8',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'G8',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'G8',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'G8',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'G8',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'G8',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'G8',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'G8',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'G8',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'G8',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'G8',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'G8',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'G8',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'G8',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'G8',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'G8',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'G8',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'G8',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'G8',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'G8',C'K3'     TERRITORY CODE                     
*                                                                               
**G8>>                                                                          
**J0>>                                                                          
*                                                                               
* SUBSIDIARY =  J0/KATZ "NAME UNKNOWN MEDIA" MASTER = KATZ RADIO GROUP          
* (ADDED MAR20/01   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'J0',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'J0',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'J0',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'J0',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'J0',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'J0',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'J0',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'J0',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'J0',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'J0',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'J0',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'J0',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'J0',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'J0',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'J0',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'J0',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'J0',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'J0',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'J0',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'J0',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'J0',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'J0',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'J0',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'J0',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'J0',C'K3'     TERRITORY CODE                     
*                                                                               
**J0>>                                                                          
**WC>>                                                                          
*                                                                               
* SUBSIDIARY =  WC/KATZ "WEST SIDE MEDIA" MASTER = KATZ RADIO GROUP             
* (ADDED SEP21/01   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'WC',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'WC',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'WC',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'WC',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'WC',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'WC',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'WC',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'WC',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'WC',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'WC',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'WC',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'WC',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'WC',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'WC',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'WC',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'WC',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'WC',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'WC',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'WC',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'WC',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'WC',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'WC',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'WC',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'WC',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'WC',C'K3'     TERRITORY CODE                     
*                                                                               
**WC>>                                                                          
*                                                                               
* SUBSIDIARY =  K4/DIMENSIONS              MASTER = KATZ RADIO GROUP            
* (ADDED SEP07/95   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'K4',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'K4',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'K4',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'K4',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'K4',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'K4',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'K4',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'K4',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'K4',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'K4',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'K4',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'K4',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'K4',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'K4',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'K4',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'K4',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'K4',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'K4',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'K4',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'K4',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'K4',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'K4',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'K4',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'K4',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'K4',C'K3'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  KU/KATZ RADIO              MASTER = KATZ RADIO GROUP            
* (ADDED SEP07/95   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'KU',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'KU',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'KU',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'KU',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'KU',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'KU',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'KU',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'KU',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'KU',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'KU',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'KU',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'KU',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'KU',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'KU',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'KU',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'KU',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'KU',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'KU',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'KU',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'KU',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'KU',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'KU',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'KU',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'KU',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'KU',C'K3'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  KF/KATZ HISPANIC           MASTER = KATZ RADIO GROUP            
* (ADDED SEP07/95   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'KF',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'KF',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'KF',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'KF',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'KF',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'KF',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'KF',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'KF',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'KF',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'KF',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'KF',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'KF',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'KF',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'KF',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'KF',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'KF',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'KF',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'KF',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'KF',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'KF',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'KF',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'KF',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'KF',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'KF',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'KF',C'K3'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  EA/EASTMAN RADIO           MASTER = KATZ RADIO GROUP            
* (ADDED SEP07/95   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'EA',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'EA',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'EA',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'EA',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'EA',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'EA',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'EA',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'EA',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'EA',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'EA',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'EA',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'EA',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'EA',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'EA',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'EA',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'EA',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'EA',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'EA',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'EA',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'EA',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'EA',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'EA',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'EA',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'EA',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'EA',C'K3'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  CR/CHRISTAL RADIO          MASTER = KATZ RADIO GROUP            
* (ADDED SEP07/95   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'CR',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'CR',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'CR',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'CR',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'CR',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'CR',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'CR',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'CR',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'CR',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'CR',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'CR',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'CR',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'CR',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'CR',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'CR',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'CR',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'CR',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'CR',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'CR',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'CR',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'CR',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'CR',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'CR',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'CR',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'CR',C'K3'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  S3/SENTRY   RADIO          MASTER = KATZ RADIO GROUP            
* (ADDED JAN05/96   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'S3',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'S3',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'S3',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'S3',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'S3',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'S3',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'S3',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'S3',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'S3',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'S3',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'S3',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'S3',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'S3',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'S3',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'S3',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'S3',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'S3',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'S3',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'S3',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'S3',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'S3',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'S3',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'S3',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'S3',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'S3',C'K3'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  K6/NOT      RADIO          MASTER = KATZ RADIO GROUP            
* (ADDED SEP05/96   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'K6',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'K6',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'K6',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'K6',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'K6',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'K6',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'K6',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'K6',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'K6',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'K6',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'K6',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'K6',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'K6',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'K6',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'K6',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'K6',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'K6',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'K6',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'K6',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'K6',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'K6',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'K6',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'K6',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'K6',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'K6',C'K3'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  RS/ABC RADIO               MASTER = KATZ RADIO GROUP            
* (ADDED MAR07/97   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'RS',C'K3'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'RS',C'K3'     SALESPERSON                        
         DC    X'46',AL1(22),C'RS',C'K3'     SALESPERSON                        
         DC    X'86',AL1(02),C'RS',C'K3'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'RS',C'K3'     GROUP                              
         DC    X'08',AL1(25),C'RS',C'K3'     ADVERTISER                         
         DC    X'88',AL1(25),C'RS',C'K3'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'RS',C'K3'     PRODUCT                            
         DC    X'89',AL1(10),C'RS',C'K3'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'RS',C'K3'     AGENCY                             
         DC    X'1A',AL1(25),C'RS',C'K3'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'RS',C'K3'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'RS',C'K3'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'RS',C'K3'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'RS',C'K3'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'RS',C'K3'     CLASS                              
         DC    X'0F',AL1(23),C'RS',C'K3'     CATEGORY                           
         DC    X'2A',AL1(22),C'RS',C'K3'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'RS',C'K3'     MARKET                             
         DC    X'8B',AL1(16),C'RS',C'K3'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'RS',C'K3'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'RS',C'K3'     POINT PERSON                       
         DC    X'3A',AL1(22),C'RS',C'K3'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'RS',C'K3'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'RS',C'K3'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  AM/KATZ AMERICAN           MASTER = KATZ TV GROUP               
* (ADDED MAR14/96   BU )                                                        
* TEAM MOVED TO MASTER LEVEL ON FEB28/99 - BILLUHR                              
*                                                                               
**NO**>> DC    X'05',AL1(23),C'AM',C'MR'     TEAM/DIVISION                      
         DC    X'05',AL1(23),C'AM',C'MR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'AM',C'MR'     SALESPERSON                        
         DC    X'46',AL1(22),C'AM',C'MR'     SALESPERSON                        
         DC    X'86',AL1(02),C'AM',C'MR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'AM',C'MR'     GROUP                              
         DC    X'08',AL1(25),C'AM',C'MR'     ADVERTISER                         
         DC    X'88',AL1(25),C'AM',C'MR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'AM',C'MR'     PRODUCT                            
         DC    X'89',AL1(10),C'AM',C'MR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'AM',C'MR'     AGENCY                             
         DC    X'1A',AL1(25),C'AM',C'MR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'AM',C'MR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'AM',C'MR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'AM',C'MR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'AM',C'MR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'AM',C'MR'     CLASS                              
         DC    X'0F',AL1(23),C'AM',C'MR'     CATEGORY                           
         DC    X'2A',AL1(22),C'AM',C'MR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'AM',C'MR'     MARKET                             
         DC    X'8B',AL1(16),C'AM',C'MR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'AM',C'MR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'AM',C'MR'     POINT PERSON                       
**NO**>> DC    X'3A',AL1(22),C'AM',C'MR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'AM',C'MR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'AM',C'MR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  CQ/KATZ CONTINENTAL        MASTER = KATZ TV GROUP               
* (ADDED MAR14/96   BU )                                                        
* TEAM MOVED TO MASTER LEVEL ON FEB28/99 - BILLUHR                              
*                                                                               
**NO**>> DC    X'05',AL1(23),C'CQ',C'MR'     TEAM/DIVISION                      
         DC    X'05',AL1(23),C'CQ',C'MR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'CQ',C'MR'     SALESPERSON                        
         DC    X'46',AL1(22),C'CQ',C'MR'     SALESPERSON                        
         DC    X'86',AL1(02),C'CQ',C'MR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'CQ',C'MR'     GROUP                              
         DC    X'08',AL1(25),C'CQ',C'MR'     ADVERTISER                         
         DC    X'88',AL1(25),C'CQ',C'MR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'CQ',C'MR'     PRODUCT                            
         DC    X'89',AL1(10),C'CQ',C'MR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'CQ',C'MR'     AGENCY                             
         DC    X'1A',AL1(25),C'CQ',C'MR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'CQ',C'MR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'CQ',C'MR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'CQ',C'MR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'CQ',C'MR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'CQ',C'MR'     CLASS                              
         DC    X'0F',AL1(23),C'CQ',C'MR'     CATEGORY                           
         DC    X'2A',AL1(22),C'CQ',C'MR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'CQ',C'MR'     MARKET                             
         DC    X'8B',AL1(16),C'CQ',C'MR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'CQ',C'MR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'CQ',C'MR'     POINT PERSON                       
**NO**>> DC    X'3A',AL1(22),C'CQ',C'MR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'CQ',C'MR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'CQ',C'MR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  V0/KATZ NON-KATZ: UNWIRED  MASTER = KATZ TV GROUP               
* (ADDED MAR19/99   BU )                                                        
* TEAM MOVED TO MASTER LEVEL ON FEB28/99 - BILLUHR                              
*                                                                               
**NO**>> DC    X'05',AL1(23),C'V0',C'MR'     TEAM/DIVISION                      
         DC    X'05',AL1(23),C'V0',C'MR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'V0',C'MR'     SALESPERSON                        
         DC    X'46',AL1(22),C'V0',C'MR'     SALESPERSON                        
         DC    X'86',AL1(02),C'V0',C'MR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'V0',C'MR'     GROUP                              
         DC    X'08',AL1(25),C'V0',C'MR'     ADVERTISER                         
         DC    X'88',AL1(25),C'V0',C'MR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'V0',C'MR'     PRODUCT                            
         DC    X'89',AL1(10),C'V0',C'MR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'V0',C'MR'     AGENCY                             
         DC    X'1A',AL1(25),C'V0',C'MR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'V0',C'MR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'V0',C'MR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'V0',C'MR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'V0',C'MR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'V0',C'MR'     CLASS                              
         DC    X'0F',AL1(23),C'V0',C'MR'     CATEGORY                           
         DC    X'2A',AL1(22),C'V0',C'MR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'V0',C'MR'     MARKET                             
         DC    X'8B',AL1(16),C'V0',C'MR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'V0',C'MR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'V0',C'MR'     POINT PERSON                       
**NO**>> DC    X'3A',AL1(22),C'V0',C'MR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'V0',C'MR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'V0',C'MR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  8K/KATZ SYNDICATED         MASTER = KATZ TV GROUP               
* (ADDED MAY10/02   BU )                                                        
* TEAM MOVED TO MASTER LEVEL ON FEB28/99 - BILLUHR                              
*                                                                               
**NO**>> DC    X'05',AL1(23),C'8K',C'MR'     TEAM/DIVISION                      
         DC    X'05',AL1(23),C'8K',C'MR'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'8K',C'MR'     SALESPERSON                        
         DC    X'46',AL1(22),C'8K',C'MR'     SALESPERSON                        
         DC    X'86',AL1(02),C'8K',C'MR'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'8K',C'MR'     GROUP                              
         DC    X'08',AL1(25),C'8K',C'MR'     ADVERTISER                         
         DC    X'88',AL1(25),C'8K',C'MR'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'8K',C'MR'     PRODUCT                            
         DC    X'89',AL1(10),C'8K',C'MR'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'8K',C'MR'     AGENCY                             
         DC    X'1A',AL1(25),C'8K',C'MR'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'8K',C'MR'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'8K',C'MR'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'8K',C'MR'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'8K',C'MR'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'8K',C'MR'     CLASS                              
         DC    X'0F',AL1(23),C'8K',C'MR'     CATEGORY                           
         DC    X'2A',AL1(22),C'8K',C'MR'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'8K',C'MR'     MARKET                             
         DC    X'8B',AL1(16),C'8K',C'MR'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'8K',C'MR'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'8K',C'MR'     POINT PERSON                       
**NO**>> DC    X'3A',AL1(22),C'8K',C'MR'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'8K',C'MR'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'8K',C'MR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  NK/KATZ NATIONAL           MASTER = KATZ TV GROUP               
* (ADDED MAR14/96   BU )                                                        
*                                                                               
**NO**>> DC    X'05',AL1(23),C'NK',C'MR'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'NK',C'MR'     SALESPERSON                        
*        DC    X'46',AL1(22),C'NK',C'MR'     SALESPERSON                        
*        DC    X'86',AL1(02),C'NK',C'MR'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'NK',C'MR'     GROUP                              
*        DC    X'08',AL1(25),C'NK',C'MR'     ADVERTISER                         
*        DC    X'88',AL1(25),C'NK',C'MR'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'NK',C'MR'     PRODUCT                            
*        DC    X'89',AL1(10),C'NK',C'MR'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'NK',C'MR'     AGENCY                             
*        DC    X'1A',AL1(25),C'NK',C'MR'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'NK',C'MR'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'NK',C'MR'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'NK',C'MR'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'NK',C'MR'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'NK',C'MR'     CLASS                              
*        DC    X'0F',AL1(23),C'NK',C'MR'     CATEGORY                           
*        DC    X'2A',AL1(22),C'NK',C'MR'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'NK',C'MR'     MARKET                             
*        DC    X'8B',AL1(16),C'NK',C'MR'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'NK',C'MR'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'NK',C'MR'     POINT PERSON                       
**NO**>> DC    X'3A',AL1(22),C'NK',C'MR'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'NK',C'MR'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'NK',C'MR'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  SUB1 (TEST)  MASTER = MASTER                                    
*                                                                               
         DC    X'05',AL1(23),C'U1',C'MS'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'U1',C'MS'     SALESPERSON                        
         DC    X'46',AL1(22),C'U1',C'MS'     SALESPERSON                        
         DC    X'86',AL1(02),C'U1',C'MS'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'U1',C'MS'     GROUP                              
         DC    X'08',AL1(25),C'U1',C'MS'     ADVERTISER                         
         DC    X'88',AL1(25),C'U1',C'MS'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'U1',C'MS'     PRODUCT                            
         DC    X'89',AL1(10),C'U1',C'MS'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'U1',C'MS'     AGENCY                             
         DC    X'1A',AL1(25),C'U1',C'MS'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'U1',C'MS'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'U1',C'MS'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'U1',C'MS'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'U1',C'MS'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'U1',C'MS'     CLASS                              
         DC    X'0F',AL1(23),C'U1',C'MS'     CATEGORY                           
         DC    X'2A',AL1(22),C'U1',C'MS'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'U1',C'MS'     MARKET                             
         DC    X'8B',AL1(16),C'U1',C'MS'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'U1',C'MS'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'U1',C'MS'     POINT PERSON                       
         DC    X'3A',AL1(22),C'U1',C'MS'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'U1',C'MS'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'U1',C'MS'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  SUB2 (TEST)  MASTER = MASTER                                    
*                                                                               
         DC    X'05',AL1(23),C'U2',C'MS'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'U2',C'MS'     SALESPERSON                        
         DC    X'46',AL1(22),C'U2',C'MS'     SALESPERSON                        
         DC    X'86',AL1(02),C'U2',C'MS'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'U2',C'MS'     GROUP                              
         DC    X'08',AL1(25),C'U2',C'MS'     ADVERTISER                         
         DC    X'88',AL1(25),C'U2',C'MS'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'U2',C'MS'     PRODUCT                            
         DC    X'89',AL1(10),C'U2',C'MS'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'U2',C'MS'     AGENCY                             
         DC    X'1A',AL1(25),C'U2',C'MS'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'U2',C'MS'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'U2',C'MS'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'U2',C'MS'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'U2',C'MS'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'U2',C'MS'     CLASS                              
         DC    X'0F',AL1(23),C'U2',C'MS'     CATEGORY                           
         DC    X'2A',AL1(22),C'U2',C'MS'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'U2',C'MS'     MARKET                             
         DC    X'8B',AL1(16),C'U2',C'MS'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'U2',C'MS'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'U2',C'MS'     POINT PERSON                       
         DC    X'3A',AL1(22),C'U2',C'MS'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'U2',C'MS'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'U2',C'MS'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  SUB3 (TEST)  MASTER = MASTER                                    
*                                                                               
         DC    X'05',AL1(23),C'UR',C'MS'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'UR',C'MS'     SALESPERSON                        
         DC    X'46',AL1(22),C'UR',C'MS'     SALESPERSON                        
         DC    X'86',AL1(02),C'UR',C'MS'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'UR',C'MS'     GROUP                              
         DC    X'08',AL1(25),C'UR',C'MS'     ADVERTISER                         
         DC    X'88',AL1(25),C'UR',C'MS'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'UR',C'MS'     PRODUCT                            
         DC    X'89',AL1(10),C'UR',C'MS'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'UR',C'MS'     AGENCY                             
         DC    X'1A',AL1(25),C'UR',C'MS'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'UR',C'MS'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'UR',C'MS'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'UR',C'MS'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'UR',C'MS'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'UR',C'MS'     CLASS                              
         DC    X'0F',AL1(23),C'UR',C'MS'     CATEGORY                           
         DC    X'2A',AL1(22),C'UR',C'MS'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'UR',C'MS'     MARKET                             
         DC    X'8B',AL1(16),C'UR',C'MS'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'UR',C'MS'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'UR',C'MS'     POINT PERSON                       
         DC    X'3A',AL1(22),C'UR',C'MS'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'UR',C'MS'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'UR',C'MS'     TERRITORY CODE                     
*                                                                               
****>>>>                                                                        
*                                                                               
*   FOLLOWING THREE REPS CONSTITUTE A TEST SETUP FOR KATZ RADIO                 
*                                                                               
* SUBSIDIARY =  6T/KATZ RADIO TEST  MASTER = KATZ TV GROUP TEST (6S)            
* (ADDED MAY05/03   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'6T',C'6S'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'6T',C'6S'     SALESPERSON                        
         DC    X'46',AL1(22),C'6T',C'6S'     SALESPERSON                        
         DC    X'86',AL1(02),C'6T',C'6S'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'6T',C'6S'     GROUP                              
         DC    X'08',AL1(25),C'6T',C'6S'     ADVERTISER                         
         DC    X'88',AL1(25),C'6T',C'6S'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'6T',C'6S'     PRODUCT                            
         DC    X'89',AL1(10),C'6T',C'6S'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'6T',C'6S'     AGENCY                             
         DC    X'1A',AL1(25),C'6T',C'6S'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'6T',C'6S'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'6T',C'6S'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'6T',C'6S'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'6T',C'6S'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'6T',C'6S'     CLASS                              
         DC    X'0F',AL1(23),C'6T',C'6S'     CATEGORY                           
         DC    X'2A',AL1(22),C'6T',C'6S'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'6T',C'6S'     MARKET                             
         DC    X'8B',AL1(16),C'6T',C'6S'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'6T',C'6S'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'6T',C'6S'     POINT PERSON                       
         DC    X'3A',AL1(22),C'6T',C'6S'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'6T',C'6S'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'6T',C'6S'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  6U/KATZ HISP  TEST  MASTER = KATZ TV GROUP TEST (6S)            
* (ADDED MAY05/03   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'6U',C'6S'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'6U',C'6S'     SALESPERSON                        
         DC    X'46',AL1(22),C'6U',C'6S'     SALESPERSON                        
         DC    X'86',AL1(02),C'6U',C'6S'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'6U',C'6S'     GROUP                              
         DC    X'08',AL1(25),C'6U',C'6S'     ADVERTISER                         
         DC    X'88',AL1(25),C'6U',C'6S'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'6U',C'6S'     PRODUCT                            
         DC    X'89',AL1(10),C'6U',C'6S'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'6U',C'6S'     AGENCY                             
         DC    X'1A',AL1(25),C'6U',C'6S'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'6U',C'6S'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'6U',C'6S'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'6U',C'6S'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'6U',C'6S'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'6U',C'6S'     CLASS                              
         DC    X'0F',AL1(23),C'6U',C'6S'     CATEGORY                           
         DC    X'2A',AL1(22),C'6U',C'6S'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'6U',C'6S'     MARKET                             
         DC    X'8B',AL1(16),C'6U',C'6S'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'6U',C'6S'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'6U',C'6S'     POINT PERSON                       
         DC    X'3A',AL1(22),C'6U',C'6S'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'6U',C'6S'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'6U',C'6S'     TERRITORY CODE                     
*                                                                               
*                                                                               
*                                                                               
* SUBSIDIARY =  6V/CHRISTAL   TEST  MASTER = KATZ TV GROUP TEST (6S)            
* (ADDED MAY05/03   BU )                                                        
*                                                                               
         DC    X'05',AL1(23),C'6V',C'6S'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'6V',C'6S'     SALESPERSON                        
         DC    X'46',AL1(22),C'6V',C'6S'     SALESPERSON                        
         DC    X'86',AL1(02),C'6V',C'6S'     SALESPERSON PASSIVE                
**NO**>> DC    X'07',AL1(23),C'6V',C'6S'     GROUP                              
         DC    X'08',AL1(25),C'6V',C'6S'     ADVERTISER                         
         DC    X'88',AL1(25),C'6V',C'6S'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'6V',C'6S'     PRODUCT                            
         DC    X'89',AL1(10),C'6V',C'6S'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'6V',C'6S'     AGENCY                             
         DC    X'1A',AL1(25),C'6V',C'6S'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'6V',C'6S'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'6V',C'6S'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'6V',C'6S'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'6V',C'6S'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'6V',C'6S'     CLASS                              
         DC    X'0F',AL1(23),C'6V',C'6S'     CATEGORY                           
         DC    X'2A',AL1(22),C'6V',C'6S'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'6V',C'6S'     MARKET                             
         DC    X'8B',AL1(16),C'6V',C'6S'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'6V',C'6S'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'6V',C'6S'     POINT PERSON                       
         DC    X'3A',AL1(22),C'6V',C'6S'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'6V',C'6S'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'6V',C'6S'     TERRITORY CODE                     
*                                                                               
****>>>>                                                                        
*                                                                               
*                                                                               
*  FOLLOWING THREE REPS CONSTITUTE A TEST SETUP FOR PLANNED                     
*     REORGANIZATION OF KATZ TV.                                                
*                                                                               
* SUBSIDIARY =  QT/AM/KATZ AMERICAN TEST      MASTER = KATZ TV GROUP            
* (ADDED JUL21/97   BU )                                                        
*                                                                               
**NO**>> DC    X'05',AL1(23),C'QT',C'YT'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'QT',C'YT'     SALESPERSON                        
*        DC    X'46',AL1(22),C'QT',C'YT'     SALESPERSON                        
*        DC    X'86',AL1(02),C'QT',C'YT'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'QT',C'YT'     GROUP                              
*        DC    X'08',AL1(25),C'QT',C'YT'     ADVERTISER                         
*        DC    X'88',AL1(25),C'QT',C'YT'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'QT',C'YT'     PRODUCT                            
*        DC    X'89',AL1(10),C'QT',C'YT'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'QT',C'YT'     AGENCY                             
*        DC    X'1A',AL1(25),C'QT',C'YT'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'QT',C'YT'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'QT',C'MS'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'QT',C'YT'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'QT',C'YT'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'QT',C'YT'     CLASS                              
*        DC    X'0F',AL1(23),C'QT',C'YT'     CATEGORY                           
*        DC    X'2A',AL1(22),C'QT',C'YT'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'QT',C'YT'     MARKET                             
*        DC    X'8B',AL1(16),C'QT',C'YT'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'QT',C'YT'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'QT',C'YT'     POINT PERSON                       
**NO**>> DC    X'3A',AL1(22),C'QT',C'YT'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'QT',C'YT'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'QT',C'YT'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY = OT/CQ/KATZ CONTINENTAL TEST   MASTER = KATZ TV GROUP             
* (ADDED JUL21/97   BU )                                                        
*                                                                               
**NO**>> DC    X'05',AL1(23),C'OT',C'YT'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'OT',C'YT'     SALESPERSON                        
*        DC    X'46',AL1(22),C'OT',C'YT'     SALESPERSON                        
*        DC    X'86',AL1(02),C'OT',C'YT'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'OT',C'YT'     GROUP                              
*        DC    X'08',AL1(25),C'OT',C'YT'     ADVERTISER                         
*        DC    X'88',AL1(25),C'OT',C'YT'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'OT',C'YT'     PRODUCT                            
*        DC    X'89',AL1(10),C'OT',C'YT'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'OT',C'YT'     AGENCY                             
*        DC    X'1A',AL1(25),C'OT',C'YT'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'OT',C'YT'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'QT',C'YT'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'OT',C'YT'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'OT',C'YT'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'OT',C'YT'     CLASS                              
*        DC    X'0F',AL1(23),C'OT',C'YT'     CATEGORY                           
*        DC    X'2A',AL1(22),C'OT',C'YT'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'OT',C'YT'     MARKET                             
*        DC    X'8B',AL1(16),C'OT',C'YT'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'OT',C'YT'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'OT',C'YT'     POINT PERSON                       
**NO**>> DC    X'3A',AL1(22),C'OT',C'YT'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'OT',C'YT'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'OT',C'YT'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  K8/NK/KATZ NATIONAL TEST      MASTER = KATZ TV GROUP            
* (ADDED JUL21/97   BU )                                                        
*                                                                               
**NO**>> DC    X'05',AL1(23),C'K8',C'YT'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'K8',C'YT'     SALESPERSON                        
*        DC    X'46',AL1(22),C'K8',C'YT'     SALESPERSON                        
*        DC    X'86',AL1(02),C'K8',C'YT'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'K8',C'YT'     GROUP                              
*        DC    X'08',AL1(25),C'K8',C'YT'     ADVERTISER                         
*        DC    X'88',AL1(25),C'K8',C'YT'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'K8',C'YT'     PRODUCT                            
*        DC    X'89',AL1(10),C'K8',C'YT'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'K8',C'YT'     AGENCY                             
*        DC    X'1A',AL1(25),C'K8',C'YT'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'K8',C'YT'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'K8',C'YT'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'K8',C'YT'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'K8',C'YT'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'K8',C'YT'     CLASS                              
*        DC    X'0F',AL1(23),C'K8',C'YT'     CATEGORY                           
*        DC    X'2A',AL1(22),C'K8',C'YT'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'K8',C'YT'     MARKET                             
*        DC    X'8B',AL1(16),C'K8',C'YT'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'K8',C'YT'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'K8',C'YT'     POINT PERSON                       
**NO**>> DC    X'3A',AL1(22),C'K8',C'YT'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'K8',C'YT'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'K8',C'YT'     TERRITORY CODE                     
*                                                                               
*                                                                               
*                                                                               
* SUBSIDIARY =  5S/SELTEL CAPITOL (SCPNYT) MASTER = SELTEL TEST                 
* (ADDED FEB02/98   BU )                             (SELNYT)                   
*                                                                               
**NO**>> DC    X'05',AL1(23),C'5S',C'4S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'5S',C'4S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'5S',C'4S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'5S',C'4S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'5S',C'4S'     GROUP                              
*        DC    X'08',AL1(25),C'5S',C'4S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'5S',C'4S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'5S',C'4S'     PRODUCT                            
*        DC    X'89',AL1(10),C'5S',C'4S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'5S',C'4S'     AGENCY                             
*        DC    X'1A',AL1(25),C'5S',C'4S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'5S',C'4S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'5S',C'4S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'5S',C'4S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'5S',C'4S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'5S',C'4S'     CLASS                              
*        DC    X'0F',AL1(23),C'5S',C'4S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'5S',C'4S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'5S',C'4S'     MARKET                             
*        DC    X'8B',AL1(16),C'5S',C'4S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'5S',C'4S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'5S',C'4S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'5S',C'4S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'5S',C'4S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'5S',C'4S'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  6S/SELTEL REPUBLIC (SRPNYT) MASTER = SELTEL TEST                
* (ADDED FEB02/98   BU )                             (SELNYT)                   
*                                                                               
**NO**>> DC    X'05',AL1(23),C'6S',C'4S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'6S',C'4S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'6S',C'4S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'6S',C'4S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'6S',C'4S'     GROUP                              
*        DC    X'08',AL1(25),C'6S',C'4S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'6S',C'4S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'6S',C'4S'     PRODUCT                            
*        DC    X'89',AL1(10),C'6S',C'4S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'6S',C'4S'     AGENCY                             
*        DC    X'1A',AL1(25),C'6S',C'4S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'6S',C'4S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'6S',C'4S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'6S',C'4S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'6S',C'4S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'6S',C'4S'     CLASS                              
*        DC    X'0F',AL1(23),C'6S',C'4S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'6S',C'4S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'6S',C'4S'     MARKET                             
*        DC    X'8B',AL1(16),C'6S',C'4S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'6S',C'4S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'6S',C'4S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'6S',C'4S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'6S',C'4S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'6S',C'4S'     TERRITORY CODE                     
*                                                                               
***>>>   TEST SETUP FOR MASTER S/P                                              
*                                                                               
*                                                                               
* SUBSIDIARY =  MCGAVREN TEST (6T)  MASTER = INTEREP                            
*                                                                               
*        DC    X'05',AL1(23),C'6T',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'6T',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'6T',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'6T',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'6T',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'6T',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'6T',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'6T',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'6T',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'6T',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'6T',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'6T',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'6T',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'6T',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'6T',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'6T',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'6T',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'6T',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'6T',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'6T',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'6T',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'6T',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'6T',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'6T',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'6T',C'7S'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  NONREP TEST (8T)  MASTER = INTEREP                              
*                                                                               
*        DC    X'05',AL1(23),C'8T',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'8T',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'8T',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'8T',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'8T',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'8T',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'8T',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'8T',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'8T',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'8T',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'8T',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'8T',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'8T',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'8T',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'8T',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'8T',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'8T',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'8T',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'8T',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'8T',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'8T',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'8T',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'8T',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'8T',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'8T',C'7S'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  CABALLERO RADIO TEST (9T) MASTER = INTEREP                      
*                                                                               
*        DC    X'05',AL1(23),C'9T',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'9T',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'9T',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'9T',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'9T',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'9T',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'9T',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'9T',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'9T',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'9T',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'9T',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'9T',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'9T',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'9T',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'9T',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'9T',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'9T',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'9T',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'9T',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'9T',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'9T',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'9T',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'9T',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'9T',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'9T',C'7S'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  INTEREP SRSNY TEST (T0)  MASTER = INTEREP                       
*                                                                               
*        DC    X'05',AL1(23),C'T0',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'T0',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'T0',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'T0',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'T0',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'T0',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'T0',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'T0',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'T0',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'T0',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'T0',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'T0',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'T0',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'T0',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'T0',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'T0',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'T0',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'T0',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'T0',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'T0',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'T0',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'T0',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'T0',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'T0',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'T0',C'7S'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  INTEREP RADIO MARKETING TEST (0T)  MASTER = INTEREP             
*                                                                               
*        DC    X'05',AL1(23),C'0T',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'0T',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'0T',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'0T',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'0T',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'0T',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'0T',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'0T',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'0T',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'0T',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'0T',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'0T',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'0T',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'0T',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'0T',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'0T',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'0T',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'0T',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'0T',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'0T',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'0T',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'0T',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'0T',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'0T',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'0T',C'7S'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  INTEREP D&R TEST (1S)  MASTER = INTEREP                         
*                                                                               
*        DC    X'05',AL1(23),C'1S',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'1S',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'1S',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'1S',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'1S',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'1S',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'1S',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'1S',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'1S',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'1S',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'1S',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'1S',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'1S',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'1S',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'1S',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'1S',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'1S',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'1S',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'1S',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'1S',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'1S',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'1S',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'1S',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'1S',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'1S',C'7S'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  INFINITY RADIO TEST (7T) MASTER = INTEREP                       
*                                                                               
*        DC    X'05',AL1(23),C'7T',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'7T',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'7T',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'7T',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'7T',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'7T',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'7T',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'7T',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'7T',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'7T',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'7T',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'7T',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'7T',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'7T',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'7T',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'7T',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'7T',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'7T',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'7T',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'7T',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'7T',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'7T',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'7T',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'7T',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'7T',C'7S'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  SHAMROCK RADIO TEST (2S)  MASTER = INTEREP                      
*                                                                               
*        DC    X'05',AL1(23),C'2S',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'2S',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'2S',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'2S',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'2S',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'2S',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'2S',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'2S',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'2S',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'2S',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'2S',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'2S',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'2S',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'2S',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'2S',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'2S',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'2S',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'2S',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'2S',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'2S',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'2S',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'2S',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'2S',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'2S',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'2S',C'7S'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  CLEAR CHANNEL RADIO TEST (3S)   MASTER = INTEREP                
*                                                                               
*        DC    X'05',AL1(23),C'3S',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'3S',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'3S',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'3S',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'3S',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'3S',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'3S',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'3S',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'3S',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'3S',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'3S',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'3S',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'3S',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'3S',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'3S',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'3S',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'3S',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'3S',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'3S',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'3S',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'3S',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'3S',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'3S',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'3S',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'3S',C'7S'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  ALLIED RADIO PARTNERS TEST (5T)   MASTER = INTEREP              
*                                                                               
*        DC    X'05',AL1(23),C'5T',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'5T',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'5T',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'5T',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'5T',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'5T',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'5T',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'5T',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'5T',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'5T',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'5T',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'5T',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'5T',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'5T',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'5T',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'5T',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'5T',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'5T',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'5T',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'5T',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'5T',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'5T',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'5T',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'5T',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'5T',C'7S'     TERRITORY CODE                     
*                                                                               
* SUBSIDIARY =  CABALLERO TV TEST (8S) MASTER = INTEREP                         
*                                                                               
*        DC    X'05',AL1(23),C'8S',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'8S',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'8S',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'8S',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'8S',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'8S',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'8S',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'8S',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'8S',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'8S',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'8S',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'8S',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'8S',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'8S',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'8S',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'8S',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'8S',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'8S',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'8S',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'8S',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'8S',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'8S',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'8S',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'8S',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'8S',C'7S'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  R0/ABC RADIO TV TEST MASTER = INTEREP                           
*                                                                               
*        DC    X'05',AL1(23),C'R0',C'7S'     TEAM/DIVISION                      
*        DC    X'06',AL1(22),C'R0',C'7S'     SALESPERSON                        
*        DC    X'46',AL1(22),C'R0',C'7S'     SALESPERSON                        
*        DC    X'86',AL1(02),C'R0',C'7S'     SALESPERSON PASSIVE                
*        DC    X'07',AL1(23),C'R0',C'7S'     GROUP                              
*        DC    X'08',AL1(25),C'R0',C'7S'     ADVERTISER                         
*        DC    X'88',AL1(25),C'R0',C'7S'     ADVERTISER D2 KEY                  
*        DC    X'09',AL1(25),C'R0',C'7S'     PRODUCT                            
*        DC    X'89',AL1(10),C'R0',C'7S'     PRODUCT D2 KEY(NETWORK #)          
*        DC    X'0A',AL1(25),C'R0',C'7S'     AGENCY                             
*        DC    X'1A',AL1(25),C'R0',C'7S'     AGENCY ADDITIONAL                  
*        DC    X'8A',AL1(25),C'R0',C'7S'     AGENCY D2 KEY                      
*        DC    X'9A',AL1(14),C'R0',C'7S'     AGENCY 9A KEY                      
*        DC    X'AA',AL1(17),C'R0',C'7S'     AGENCY AA KEY                      
*        DC    X'BA',AL1(25),C'R0',C'7S'     AGENCY ADDITIONAL D2 KEY           
*        DC    X'0D',AL1(23),C'R0',C'7S'     CLASS                              
*        DC    X'0F',AL1(23),C'R0',C'7S'     CATEGORY                           
*        DC    X'2A',AL1(22),C'R0',C'7S'     OWNERSHIP                          
*        DC    X'2B',AL1(21),C'R0',C'7S'     MARKET                             
*        DC    X'8B',AL1(16),C'R0',C'7S'     MARKET D2 KEY                      
*        DC    X'AF',AL1(02),C'R0',C'7S'     MARKET D2 KEY                      
*        DC    X'31',AL1(22),C'R0',C'7S'     POINT PERSON                       
*        DC    X'3A',AL1(22),C'R0',C'7S'     DEVELOPMENTAL S/PERSON             
*        DC    X'3B',AL1(23),C'R0',C'7S'     DEVELOPMENTAL CON TYPE             
*        DC    X'3D',AL1(23),C'R0',C'7S'     TERRITORY CODE                     
*                                                                               
*NATL>>>                                                                        
*                                                                               
* SUBSIDIARY =  NATL PUBLIC TV   MASTER = NATPUB MASTER                         
*                                                                               
         DC    X'05',AL1(23),C'NP',C'V6'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'NP',C'V6'     SALESPERSON                        
         DC    X'46',AL1(22),C'NP',C'V6'     SALESPERSON                        
         DC    X'86',AL1(02),C'NP',C'V6'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'NP',C'V6'     GROUP                              
         DC    X'08',AL1(25),C'NP',C'V6'     ADVERTISER                         
         DC    X'88',AL1(25),C'NP',C'V6'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'NP',C'V6'     PRODUCT                            
         DC    X'89',AL1(10),C'NP',C'V6'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'NP',C'V6'     AGENCY                             
         DC    X'1A',AL1(25),C'NP',C'V6'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'NP',C'V6'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'NP',C'V6'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'NP',C'V6'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'NP',C'V6'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'NP',C'V6'     CLASS                              
         DC    X'0F',AL1(23),C'NP',C'V6'     CATEGORY                           
         DC    X'2A',AL1(22),C'NP',C'V6'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'NP',C'V6'     MARKET                             
         DC    X'8B',AL1(16),C'NP',C'V6'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'NP',C'V6'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'NP',C'V6'     POINT PERSON                       
         DC    X'3A',AL1(22),C'NP',C'V6'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'NP',C'V6'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'NP',C'V6'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  NATL PUBLIC RADIO  MASTER = NATPUB MASTER                       
*                                                                               
         DC    X'05',AL1(23),C'V5',C'V6'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'V5',C'V6'     SALESPERSON                        
         DC    X'46',AL1(22),C'V5',C'V6'     SALESPERSON                        
         DC    X'86',AL1(02),C'V5',C'V6'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'V5',C'V6'     GROUP                              
         DC    X'08',AL1(25),C'V5',C'V6'     ADVERTISER                         
         DC    X'88',AL1(25),C'V5',C'V6'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'V5',C'V6'     PRODUCT                            
         DC    X'89',AL1(10),C'V5',C'V6'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'V5',C'V6'     AGENCY                             
         DC    X'1A',AL1(25),C'V5',C'V6'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'V5',C'V6'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'V5',C'V6'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'V5',C'V6'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'V5',C'V6'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'V5',C'V6'     CLASS                              
         DC    X'0F',AL1(23),C'V5',C'V6'     CATEGORY                           
         DC    X'2A',AL1(22),C'V5',C'V6'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'V5',C'V6'     MARKET                             
         DC    X'8B',AL1(16),C'V5',C'V6'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'V5',C'V6'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'V5',C'V6'     POINT PERSON                       
         DC    X'3A',AL1(22),C'V5',C'V6'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'V5',C'V6'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'V5',C'V6'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  NATL PUBLIC TV TEST      MASTER = NATPUB TEST MASTER            
*                                                                               
         DC    X'05',AL1(23),C'QB',C'VD'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'QB',C'VD'     SALESPERSON                        
         DC    X'46',AL1(22),C'QB',C'VD'     SALESPERSON                        
         DC    X'86',AL1(02),C'QB',C'VD'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'QB',C'VD'     GROUP                              
         DC    X'08',AL1(25),C'QB',C'VD'     ADVERTISER                         
         DC    X'88',AL1(25),C'QB',C'VD'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'QB',C'VD'     PRODUCT                            
         DC    X'89',AL1(10),C'QB',C'VD'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'QB',C'VD'     AGENCY                             
         DC    X'1A',AL1(25),C'QB',C'VD'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'QB',C'VD'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'QB',C'VD'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'QB',C'VD'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'QB',C'VD'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'QB',C'VD'     CLASS                              
         DC    X'0F',AL1(23),C'QB',C'VD'     CATEGORY                           
         DC    X'2A',AL1(22),C'QB',C'VD'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'QB',C'VD'     MARKET                             
         DC    X'8B',AL1(16),C'QB',C'VD'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'QB',C'VD'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'QB',C'VD'     POINT PERSON                       
         DC    X'3A',AL1(22),C'QB',C'VD'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'QB',C'VD'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'QB',C'VD'     TERRITORY CODE                     
*                                                                               
*                                                                               
* SUBSIDIARY =  NATL PUBLIC RADIO TEST     MASTER = NATPUB TEST MASTER          
*                                                                               
         DC    X'05',AL1(23),C'VZ',C'VD'     TEAM/DIVISION                      
         DC    X'06',AL1(22),C'VZ',C'VD'     SALESPERSON                        
         DC    X'46',AL1(22),C'VZ',C'VD'     SALESPERSON                        
         DC    X'86',AL1(02),C'VZ',C'VD'     SALESPERSON PASSIVE                
         DC    X'07',AL1(23),C'VZ',C'VD'     GROUP                              
         DC    X'08',AL1(25),C'VZ',C'VD'     ADVERTISER                         
         DC    X'88',AL1(25),C'VZ',C'VD'     ADVERTISER D2 KEY                  
         DC    X'09',AL1(25),C'VZ',C'VD'     PRODUCT                            
         DC    X'89',AL1(10),C'VZ',C'VD'     PRODUCT D2 KEY(NETWORK #)          
         DC    X'0A',AL1(25),C'VZ',C'VD'     AGENCY                             
         DC    X'1A',AL1(25),C'VZ',C'VD'     AGENCY ADDITIONAL                  
         DC    X'8A',AL1(25),C'VZ',C'VD'     AGENCY D2 KEY                      
         DC    X'9A',AL1(14),C'VZ',C'VD'     AGENCY 9A KEY                      
         DC    X'AA',AL1(17),C'VZ',C'VD'     AGENCY AA KEY                      
         DC    X'BA',AL1(25),C'VZ',C'VD'     AGENCY ADDITIONAL D2 KEY           
         DC    X'0D',AL1(23),C'VZ',C'VD'     CLASS                              
         DC    X'0F',AL1(23),C'VZ',C'VD'     CATEGORY                           
         DC    X'2A',AL1(22),C'VZ',C'VD'     OWNERSHIP                          
         DC    X'2B',AL1(21),C'VZ',C'VD'     MARKET                             
         DC    X'8B',AL1(16),C'VZ',C'VD'     MARKET D2 KEY                      
         DC    X'AF',AL1(02),C'VZ',C'VD'     MARKET D2 KEY                      
         DC    X'31',AL1(22),C'VZ',C'VD'     POINT PERSON                       
         DC    X'3A',AL1(22),C'VZ',C'VD'     DEVELOPMENTAL S/PERSON             
         DC    X'3B',AL1(23),C'VZ',C'VD'     DEVELOPMENTAL CON TYPE             
         DC    X'3D',AL1(23),C'VZ',C'VD'     TERRITORY CODE                     
*                                                                               
*NATL>>>                                                                        
         DC    X'00',AL1(00)                 END OF TABLE                       
*                                                                               
         DC    (09900-(*-INTEREP))X'00'      PAD THE HOLE WITH NULLS            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025INTEREPS  05/05/03'                                      
         END                                                                    
