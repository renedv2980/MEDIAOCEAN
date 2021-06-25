*          DATA SET TAGEN66    AT LEVEL 011 AS OF 08/12/03                      
*PHASE T70266A                                                                  
         TITLE 'T70266 - TABLES FOR 1983 CONTRACTS'                             
T70266   CSECT                                                                  
         DC    AL4(USETBLS-T70266)                                              
         DC    AL4(USELUT-T70266)                                               
         DC    A(0)                                                             
         DC    AL4(AFMCOLS-T70266)                                              
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
MUSTAB   DC    AL1(35,16,0,0)      REUSE                                        
         DC    F'5250'             CAST=1                                       
         DC    F'5673'                  2-4                                     
         DC    F'5250'                  5+                                      
         SPACE 3                                                                
BSMTAB   DC    AL1(60,16,0,0)      SESSION                                      
         DC    F'7000'             CAST=1                                       
         DC    F'7565'                  2-4                                     
         DC    F'7000'                  5+                                      
         SPACE 3                                                                
FGMTAB   DC    AL1(96,16,0,0)      FOREIGN USE                                  
         DC    F'10500'            CAST=1                                       
         DC    F'11346'                 2-4                                     
         DC    F'10500'                 5+                                      
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL1(60,UBSM,ALL,AFM+NON,0,0,0,ALL)     SESSION                   
*                                                                               
         DC    AL1(35,UMUS,UMUSDUB,ALL,0,0,0,ALL)     DUBBING                   
         DC    AL1(35,UMUS,UMUS13W,ALL,0,0,0,ALL)     REUSE                     
         DC    AL1(35,UMUS,UMUSNEW,ALL,0,0,0,ALL)     NEW                       
*                                                                               
* NO-OP  DC    AL1(96,UFGM,0,ALL,0,0,0,ALL)           FOREIGN USE               
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - AFM                                              
         SPACE 3                                                                
AFMCOLS  DS    0CL2                                                             
         DC    AL1(1,CTA)          ARRANGER                                     
         DC    AL1(1,CTACP)        ARRANGER-COPYIST                             
         DC    AL1(1,CTAM)         ARRANGER-MUSICIAN                            
         DC    AL1(1,CTAMC)        ARRANGER-MUSICIAN-COPYIST                    
         DC    AL1(1,CTAO)         ARRANGER-ORCHESTRATOR                        
*                                                                               
         DC    AL1(1,CTC)          CONTRACTOR                                   
         DC    AL1(1,CTCO)         CONTRACTOR-ORCHESTRATOR                      
         DC    AL1(1,CTCCP)        CONTRACTOR-COPYIST                           
*                                                                               
         DC    AL1(1,CTCP)         COPYIST                                      
*                                                                               
         DC    AL1(1,CTL)          LEADER                                       
         DC    AL1(1,CTLA)         LEADER-ARRANGER                              
         DC    AL1(1,CTLAC)        LEADER-ARRANGER-COPYIST                      
         DC    AL1(1,CTLCP)        LEADER-COPYIST                               
         DC    AL1(1,CTLM)         LEADER-MUSICIAN                              
         DC    AL1(1,CTLO)         LEADER-ORCHESTRATOR                          
         DC    AL1(1,CTLOC)        LEADER-ORCHESTRATOR-COPYIST                  
*                                                                               
         DC    AL1(1,CTM)          MUSICIAN                                     
         DC    AL1(1,CTMCP)        MUSICIAN-COPYIST                             
*                                                                               
         DC    AL1(1,CTO)          ORCHESTRATOR                                 
         DC    AL1(1,CTOCP)        ORCHESTRATOR-COPYIST                         
         DC    AL1(1,CTOM)         ORCHESTRATOR-MUSICIAN                        
         DC    AL1(1,CTOMC)        ORCHESTRATOR-MUSICIAN-COPYIST                
*                                                                               
         DC    AL1(1,CTSYN)        SYNTHESIZER                                  
*                                                                               
         DC    AL1(1,CTZZZ)        GENERAL                                      
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TASYSEQUS                                                      
         EJECT                                                                  
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011TAGEN66   08/12/03'                                      
         END                                                                    
