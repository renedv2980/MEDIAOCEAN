*          DATA SET REREP8501  AT LEVEL 022 AS OF 08/31/00                      
*          DATA SET REREP8501  AT LEVEL 021 AS OF 05/15/87                      
*PHASE RE8501A                                                                  
         TITLE 'SPECS FOR BUSINESS OPPORTUNITY REPORT'                          
RE8501   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         FSPEC GET,ADVERTISER                                                   
         FSPEC GET,AGENCY                                                       
         FSPEC GET,PRODUCT                                                      
         FSPEC GET,SALESMAN                                                     
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,41,C'BUSINESS OPPORTUNITIES REPORT'                           
         ASPEC H1,85,RENUM                                                      
         ASPEC H1,98,PAGE                                                       
         ASPEC H2,41,29C'-'                                                     
*        ASPEC H3,2,REP       ***TEMP OUT FOR BLAIR RADIO NAME CHANGE**         
         SPACE 1                                                                
         SPROG 0,1                                                              
         ASPEC H1,1,RUN                                                         
         ASPEC H1,51,C'BUSINESS OPPORTUNITIES REPORT'                           
         ASPEC H2,51,29C'-'                                                     
         ASPEC H1,109,RENUM                                                     
         ASPEC H1,125,PAGE                                                      
*        ASPEC H3,1,REP       ***TEMP OUT FOR BLAIR RADIO NAME CHANGE**         
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H6,1,C'ADVERTISER           AGENCY'                              
         ASPEC H7,1,C'PRODUCT              OFFICE'                              
         ASPEC H8,1,C'SCHEDULE             SALESPERSON'                         
         SPACE 1                                                                
         ASPEC H6,43,C'PREFERRED TIMES      AUDIENCE'                           
         ASPEC H7,43,C'BUYING OBJECTIVES    RATING SERVICE'                     
         ASPEC H8,43,C'MER? NETWORK? ORDER? BOOK'                               
         ASPEC H6,85,C'MARKETS'                                                 
         ASPEC H7,85,C'-------'                                                 
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H4,58,C'(MARKET RECAP)'                                          
         ASPEC H6,1,C'MARKET'                                                   
         ASPEC H6,22,C'ADVERTISER           AGENCY'                             
         ASPEC H7,22,C'PRODUCT              OFFICE'                             
         ASPEC H8,22,C'SCHEDULE             SALESPERSON'                        
         SPACE 1                                                                
         ASPEC H6,64,C'PREFERRED TIMES      AUDIENCE'                           
         ASPEC H7,64,C'BUYING OBJECTIVES    RATING SERVICE'                     
         ASPEC H8,64,C'MER? NETWORK? ORDER? BOOK'                               
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H3,50,C'DEMO SUMMARY'                                            
         ASPEC H6,2,C'DEMO'                                                     
         ASPEC H6,22,C'PRIMARY'                                                 
         ASPEC H6,35,C'SECONDARY'                                               
         ASPEC H7,2,4C'-'                                                       
         ASPEC H7,22,7C'-'                                                      
         ASPEC H7,35,9C'-'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022REREP8501 08/31/00'                                      
         END                                                                    
