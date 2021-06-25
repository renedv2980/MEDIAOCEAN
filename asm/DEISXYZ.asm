*          DATA SET DEISXYZ    AT LEVEL 002 AS OF 11/21/00                      
*PHASE DEISXYZA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
         TITLE 'DATAMGR EXAMPLE'                                                
DEISDMGR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DEISDMGR,=V(REGSAVE)                                           
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
* THE DMOPEN COMMAND IS ONE YOU WILL NEVER SEE AGAIN                            
*                                                                               
MAIN     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'SPOT',               +        
               =C'NSTAFILEX',IO,0                                               
*                                                                               
* PRINT ALL THE MARKET RECORDS FOR MEDIA 'R' (RADIO), AGENCY 'SJ'               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKTRECD,R4                                                       
         MVI   MKTKTYPE,MKTKTYPQ   READ FIRST MARKET RECORD. . .                
         MVI   MKTKMED,C'R'        . . . FOR RADIO                              
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,IO               
*                                                                               
LOOP     CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         CLC   KEY(MKTKMKT-MKTKEY),MKTKEY  SAME TYPE/MEDIA?                     
         BNE   GOODBYE                                                          
*                                                                               
         CLC   MKTKAGY,=C'SJ'              AGENCY SJR?                          
         BNE   NEXT                                                             
*                                                                               
         MVC   P(L'MKTKEY),MKTKEY  PRINT THE KEY                                
         MVC   P+20(L'MKTNAME),MKTNAME   AND THE MARKET NAME                    
         DROP  R4                                                               
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'STATION',KEY,IO               
         B     LOOP                                                             
*                                                                               
GOODBYE  XBASE                                                                  
         EJECT                                                                  
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                                     
*                                                                               
UTL      DC    F'0',X'02'          FOR OFFLINE DATAMGR                          
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
KEY      DS    CL15                STAFILE KEY                                  
IO       DS    1000X               I/O AREA FOR STAFILE                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DEISXYZ   11/21/00'                                      
         END                                                                    
