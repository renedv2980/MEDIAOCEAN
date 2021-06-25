*          DATA SET DDPQGETTST AT LEVEL 011 AS OF 02/27/91                      
*PHASE PQGETTST                                                                 
*INCLUDE PQGETLIN                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'DDPQGETTST -- TEST MODULE PQGETLIN'                             
PQGETTST CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PQGETTST,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         LA    R4,KEY                                                           
         USING UKRECD,R4                                                        
         MVC   UKSRCID,=AL2(1359)  USERID (OMNYA)                               
         MVC   UKSUBID,=C'P91'     SUB-ID                                       
*********MVC   UKREPNO,=AL2(47)    REPORT ID                                    
         DROP  R4                                                               
*                                                                               
LOOP     GOTO1 =V(PQGETLIN),DMCB,KEY,R                                          
         CLI   DMCB,0              LINE FOUND?                                  
         BE    PRINTLIN            YES -- PRINT IT                              
         CLI   DMCB,2              END OF FILE?                                 
         BE    GOODBYE             YES                                          
         CLI   DMCB,1              REPORT NOT FOUND?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(16),=C'REPORT NOT FOUND'                                       
         GOTO1 =V(PRINTER)                                                      
         B     GOODBYE                                                          
*                                                                               
PRINTLIN MVC   P,R+1               IGNORE CARRIAGE CONTROL                      
         GOTO1 =V(PRINTER)         PRINT EACH LINE                              
         B     LOOP                                                             
*                                                                               
GOODBYE  XBASE                                                                  
         EJECT                                                                  
DUMPLIST DS    0F                                                               
         DC    A(PQGETTST),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
DMCB     DS    6F                                                               
SSB      DC    F'0'                FOR DATAMGR (OFFLINE)                        
UTL      DC    F'0',X'07'          FOR DATAMGR (TALENT SYSTEM)                  
KEY      DC    XL7'00'             PQ KEY                                       
R        DS    CL200               PQ RECORD                                    
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011DDPQGETTST02/27/91'                                      
         END                                                                    
