*          DATA SET REREP2402V AT LEVEL 003 AS OF 06/08/98                      
*PHASE RE2402C,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO PURGE INV RECORDS'                                    
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
         XC    SVKEY,SVKEY                                                      
         XC    COUNT,COUNT                                                      
         XC    MYFLAG,MYFLAG                                                    
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   KEY,X'12'                                                        
         MVC   KEY+10(2),=C'B3'    BLRNY <============                          
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
         LA    R6,KEY                                                           
         CLI   KEY,X'12'                                                        
         BNE   PCX                                                              
         CLC   KEY+10(2),=C'B3'    STILL BLRNY? <==========                     
         BNE   PCX                                                              
*                                                                               
         CLI   RINVKSRC,C'M'       MARKET FACT?                                 
         BE    PCSEQ                                                            
         CLI   RINVKSRC,C'S'       STATION FACT?                                
         BE    PCSEQ                                                            
*                                                                               
         CLI   RINVKSRC,0          INVENTORY HEADER?                            
         BNE   PC30                                                             
         MVC   SVKEY,KEY                                                        
         NI    MYFLAG,X'FF'-DELREC                                              
         B     PC50                                                             
*                                                                               
PC30     OC    SVKEY,SVKEY         DID WE GET A HEADER YET?                     
         BZ    PCSEQ               NO (ONLY UNTIL 1ST HEADER FOUND)             
*                                                                               
         CLC   RINVKEY(24),SVKEY   SAME UP TO EFF DATE?                         
         BNE   PCSEQ                                                            
*                                                                               
         TM    MYFLAG,DELREC       DELETE THIS RECORD?                          
         BZ    PCSEQ                                                            
*                                                                               
PC50     GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,IOAREA,DMWORK             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
*                                                                               
         CLI   RINVKSRC,0          HEADER RECORD?                               
         BNE   PC100               NO                                           
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2         ANY INV END?                    
         BZ    PCSEQ                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(3,ENDEFFD)                           
         CLC   ENDEFFD,PURGEDAT    BEFORE JAN 1/97?                             
         BNL   PCSEQ                                                            
         OI    MYFLAG,DELREC                                                    
*                                                                               
PC100    DS    0H                                                               
         OI    29(R6),X'80'        MARK RECORD FOR DELETION                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,IOAREA,DMWORK             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+27,X'80'        MARK KEY FOR DELETION                        
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DMWRT),REPDIR,KEY,KEY                            
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RINVKSRC,0          HEADER RECORD?                               
         BNE   PC150                                                            
*                                                                               
         MVC   P(11),RINVKREP                                                   
         GOTO1 HEXOUT,DMCB,RINVKSTD,P+15,6,=C'TOG'                              
         GOTO1 REPORT                                                           
*                                                                               
PC150    L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
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
REPLIST  DC    C'B3'                                                            
         DC    X'0000'                                                          
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
*                                                                               
IOAREA   DS    XL2000                                                           
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
**PAN#1  DC    CL21'003REREP2402V06/08/98'                                      
         END                                                                    
