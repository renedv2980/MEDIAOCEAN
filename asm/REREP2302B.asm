*          DATA SET REREP2302B AT LEVEL 059 AS OF 05/01/02                      
*PHASE RE2302B,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO FIND BAD PASSIVE KEYS'                                
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2302 (RE2302) --- REP FILE MARKER                   *             
*                                                                 *             
* SCHT - THIS MODULE FINDS ALL BAD PASSIVE KEYS (X'92')           *             
*        FOR INVENTORY RECORDS                                    *             
*                                                                 *             
* --------------------------------------------------------------- *             
*******************************************************************             
*                                                                               
RE2302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2302,R9,RR=R5                                              
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
*                                                                               
         LA    R6,KEY                                                           
         MVI   KEY,X'92'                                                        
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLI   KEY,X'92'                                                        
         BNE   PCX                                                              
         USING RIDPKTYP,R6                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IOAREA                                                        
         USING REINVREC,R4                                                      
*                                                                               
         CLC   RIDPKREP,RINVKREP   SAME REP?                                    
         BNE   PC30                                                             
         CLC   RIDPKSTA,RINVKSTA   SAME STATION?                                
         BNE   PC30                                                             
         CLC   RIDPKINV,RINVKINV   SAME INV#?                                   
         BNE   PC30                                                             
         CLC   RIDPKSTD,RINVKSTD   SAME EFF DATE?                               
         BNE   PC30                                                             
*                                                                               
         CLC   KEY+27(1),29(R4)    SAME STATUS CODE?                            
         BE    PCSEQ                                                            
*                                                                               
         CLI   29(R4),X'80'        WAS HEADER MARKED DELETED?                   
         BNE   PC40                                                             
         MVC   P+50(16),=C'<=== HDR DELETED'                                    
         B     PC40                                                             
*                                                                               
PC30     DS    0H                                                               
         MVC   P+50(17),=C'<=== NOT SAME KEY'                                   
*                                                                               
PC40     DS    0H                                                               
         MVC   P(2),RINVKREP                                                    
         MVC   P+3(5),RINVKSTA                                                  
         MVC   P+10(4),RINVKINV                                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,RINVKSTD,P+16,3,=C'TOG'                              
*                                                                               
         MVC   P+25(1),RIDPKDPT    DAYPART                                      
         DROP  R4                                                               
*                                                                               
         LA    R4,IOAREA           PRINT STATUS DATES                           
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING RINVAEL,R4                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,RINVAFST),(5,P+30)                                
         GOTO1 DATCON,DMCB,(3,RINVALST),(5,P+40)                                
         DROP  R4                                                               
*                                                                               
         LA    R4,IOAREA                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
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
         GETEL R4,34,ELCODE                                                     
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
         DC    A(RE2302,65000)                                                  
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
ELCODE   DS    XL1                                                              
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
**PAN#1  DC    CL21'059REREP2302B05/01/02'                                      
         END                                                                    
