*          DATA SET DEISDM4    AT LEVEL 018 AS OF 11/09/00                      
*PHASE DEISDM4A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
         TITLE 'ANOTHER DATAMGR EXAMPLE'                                        
DEISDMGR CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,DEISDMGR,=V(REGSAVE),R9                                        
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(DEISDMGR),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
* DON'T WORRY ABOUT THIS DMOPEN CALL                                            
*                                                                               
MAIN     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IO,0                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD PHASE RECORD KEY                       
         USING CTPHRECD,R4                                                      
         MVI   CTPHID,CTPHIDQ                                                   
         MVI   CTPHSUBI,CTPHSUBQ                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
*                                                                               
LOOP     CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BAD RETURN CODE FROM DATAMGR                 
*                                                                               
         LA    R4,IO                                                            
         CLC   KEY(CTPHNAME-CTPHPKEY),CTPHPKEY   STILL A PHASE RECORD?          
         BNE   GOODBYE                           NO                             
*                                                                               
         CLI   CTPHLVL,0           IS THIS A TEST LEVEL?                        
         BE    NEXT                YES                                          
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,IO,P,25,=C'TOG'  PRINT THE HEX KEY               
         CLC   =F'50',16(R1)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,CTPHFRST         POINT TO FIRST ELEMENT                       
         CLI   0(R3),CTPHSCEQ      BETTER BE THE '05' ELEMENT                   
         BE    *+6                                                              
         DC    H'0'                IT ISN'T                                     
*                                                                               
         USING CTPHSYSD,R3                                                      
         TM    CTPHSFL1,CTPHSSCQ   IS IT A SCREEN?                              
         BO    *+14                YES                                          
         MVC   P+55(7),=C'PROGRAM' NO                                           
         B     *+10                                                             
         MVC   P+55(6),=C'SCREEN'                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IO                
         B     LOOP                                                             
*                                                                               
GOODBYE  XBASE                                                                  
         EJECT                                                                  
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                                     
*                                                                               
UTL      DC    F'0',X'0A'          FOR OFFLINE DATAMGR                          
*                                                                               
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
DMCB     DS    6F                                                               
KEY      DS    CL25                CTFILE KEY                                   
IO       DS    1000X               I/O AREA FOR CTFILE                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENPHASE                                                     
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018DEISDM4   11/09/00'                                      
         END                                                                    
