*          DATA SET REREP2402W AT LEVEL 045 AS OF 06/01/99                      
*PHASE RE2402W,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO FIND BAD PASSIVE KEYS'                                
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
*******************************************************************             
*                                                                               
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    COUNT,COUNT                                                      
         XC    COUNTER,COUNTER                                                  
         XC    COUNTER1,COUNTER1                                                
*                                                                               
         LA    R6,KEY                                                           
         MVI   KEY,X'8D'                                                        
*        MVC   KEY+1(2),=C'BL'                                                  
*        MVC   KEY+1(2),=C'PV'                                                  
*        MVC   KEY+8(2),=X'C6D8'                                                
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         L     RF,COUNTER                                                       
         LA    RF,1(RF)                                                         
         ST    RF,COUNTER                                                       
         L     RF,COUNTER1                                                      
         LA    RF,1(RF)                                                         
         ST    RF,COUNTER1                                                      
         CLC   COUNTER1,=F'10000'                                               
         BNE   PC100                                                            
         XC    COUNTER1,COUNTER1                                                
         MVC   P+1(16),=C'PROCESSING RECS:'                                     
         EDIT  COUNTER,(7,P+20)                                                 
         MVC   P+30(32),KEY                                                     
         GOTO1 REPORT                                                           
*                                                                               
PC100    DS    0H                                                               
         CLI   KEY,X'8D'                                                        
         BH    PCX                                                              
*        CLC   KEY+1(2),=C'PV'                                                  
*        BH    PCX                                                              
         CLI   KEY+16,X'01'                                                     
         BNE   PCSEQ                                                            
         CLI   KEY+10,2                                                         
         BH    PCSEQ                                                            
* FOUND ONE!!!!                                                                 
PC50     DS    0H                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
         MVC   P(5),=C'BAD: '                                                   
         MVC   P+6(32),KEY                                                      
         GOTO1 =V(HEXOUT),DMCB,KEY+12,P+44,4,=C'TOG'                            
         GOTO1 REPORT                                                           
         B     PCSEQ                                                            
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT ======>'                                          
*                                                                               
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
DELREC   EQU   X'01'               DELETE THIS RECORD                           
*                                                                               
COUNT    DS    F                                                                
COUNTER  DS    F                                                                
COUNTER1 DS    F                                                                
SVKEY    DS    CL27                                                             
*                                                                               
PURGEKEY DS    CL24                                                             
*                                                                               
ENDEFFD  DS    XL3                 INV END DATE (BINARY)                        
PURGEDAT DC    XL3'610101'         JAN 1,1997                                   
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
REPLIST  DC    C'BL'                                                            
         DC    X'0000'                                                          
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
MYKEY    DS    CL32                                                             
*                                                                               
IOAREA   DS    XL4000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045REREP2402W06/01/99'                                      
         END                                                                    
