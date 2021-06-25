*          DATA SET JHIGDM1    AT LEVEL 046 AS OF 10/23/00                      
*PHASE JHIGDM1                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
         TITLE 'DATAMGR EXAMPLE'                                                
JHIGDMGR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,JHIGDMGR,=V(REGSAVE)                                           
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
         DC    A(JHIGDMGR),V(DUMMY)                                             
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
         USING STARECD,R4                                                       
         LA    R4,KEY                                                           
         MVI   STAKTYPE,STAKTYPQ   READ FIRST MARKET RECORD. . .                
         MVI   STAKMED,C'R'        . . . FOR RADIO                              
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,IO               
*                                                                               
LOOP     CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         CLC   KEY(STAKCALL-STAKEY),STAKEY SAME TYPE/MEDIA?                     
         BNE   BUILT                                                            
*                                                                               
         CLC   STAKAGY,=C'SJ'              AGENCY SJR?                          
         BNE   NEXT                                                             
*                                                                               
         CLC   STAKCALL+4(1),=C'A'                                              
         BNE   NEXT                                                             
*                                                                               
         CLC   STAKCLT,=3C'0'                                                   
         BNE   NEXT                                                             
*                                                                               
         MVC   LEN,STAKLEN                                                      
         MVC   ZER,=X'0000'                                                     
         MVC   STAMKT,SMKT                                                      
         MVC   STACALL,STAKCALL                                                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',TEMP                                     
         B     NEXT                                                             
*                                                                               
BUILT    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BZ    GOODBYE                                                          
         LA    R8,TEMP                                                          
         LHI   R7,L'TEMP                                                        
         LHI   R9,L'TEMP                                                        
         MVCL  R8,R6                                                            
         MVC   P(L'STAKEY),IO                                                   
         MVC   P+20(L'STAMKT),STAMKT                                            
***************************************************************                 
*        MVC   P(L'STAKEY),STAKEY  PRINT THE KEY                                
*        MVC   P+20(L'SMKT),SMKT   AND MARKET CODE                              
*                                                                               
*        MVC   KEY,STAKEY                SAVE OFF CURRENT KEY                   
*                                                                               
         USING MKTRECD,R5                                                       
         LA    R5,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVI   MKTKTYPE,MKTKTYPQ         BUILD KEY TO RETRIEVE MKTNAME          
         MVI   MKTKMED,C'R'                                                     
         MVC   MKTKMKT,STAMKT                                                   
         MVC   MKTKAGY,=C'SJ'                                                   
         MVC   MKTKFILL,=7C'0'                                                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'STATION',KEY2,IO              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,IO                                                            
         MVC   P+25(L'MKTNAME),MKTNAME   PRINT MARKET NAME                      
*                                                                               
* GO BACK TO CURRENT STATION RECORD                                             
*                                                                               
*        MVC   KEY,KEY3                                                         
*        GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'STATION',KEY,IO               
*        CLI   8(R1),0                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
         DROP  R4                                                               
         DROP  R5                                                               
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         B     BUILT                                                            
*                                                                               
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'STATION',KEY,IO               
         B     LOOP                                                             
*                                                                               
GOODBYE  DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         XBASE                                                                  
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
KEY2     DS    CL15                                                             
TEMP     DS    0CL365                                                           
LEN      DS    XL2                                                              
ZER      DS    XL2                                                              
SORTKEY  DS    0CL9                                                             
STAMKT   DS    CL4                                                              
STACALL  DS    CL5                                                              
IO       DS    352X                I/O AREA FOR STAFILE                         
TEMPLQ   EQU   *-TEMP                                                           
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=365'                                   
SORTCARD DC    CL80'SORT FIELDS=(5,9,A),FORMAT=BI,WORK=1'                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046JHIGDM1   10/23/00'                                      
         END                                                                    
