*          DATA SET REREP2402U AT LEVEL 064 AS OF 05/01/02                      
*PHASE RE2402U,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'CHANGE OLD RATE TO NEW STYLE RATE CARD'                         
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
         L     RF,=V(HELLO)                                                     
         L     RE,RELO                                                          
         AR    RF,RE                                                            
         ST    RF,VHELLO                                                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
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
         USING RARTKEY,R6                                                       
         MVI   KEY,X'3E'                                                        
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
         USING RARTKEY,R6                                                       
         CLI   KEY,X'3E'                                                        
         BNE   PCX                                                              
*                                                                               
         CLI   RARTKCOD+4,0        OLD STYLE RATE                               
         BNE   PCSEQ                                                            
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(2),RARTKREP                                                    
         MVC   P+3(4),RARTKCOD                                                  
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,OLDRATE,DMWORK            
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
******************************************************************              
*       BUILD NEW STYLE RATE RECORD                                             
******************************************************************              
         LA    R6,OLDRATE          ORIGINAL RATE RECORD                         
         LA    R3,NEWRATE          BUILD NEW STYLE RATE RECORD                  
         XCEF  (R3),2000                                                        
         USING RARTKEY,R3                                                       
*                                                                               
         MVC   0(L'RARTKEY,R3),0(R6)      COPY RATE KEY                         
         OC    RARTKCOD,=C'        '  MAKE IT NEW STYLE RATE                    
*                                                                               
         MVC   RARTLEN,=H'34'      REC LENGTH                                   
*                                                                               
         MVI   ELCODE,X'01'        GET PROGRAM ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R4,1(R6)            ELEMENT LENGTH                               
         BCTR  R4,0                                                             
*                                                                               
         EX    R4,*+8              COPY ELEMENT                                 
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',REPFILE),(R3),ELEM,=C'ADD=CODE'                
*                                                                               
         LA    R6,OLDRATE                                                       
         MVI   ELCODE,X'02'        GET LENGTH/QTR ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   PC150                                                            
         USING RALQELEM,R6                                                      
*                                                                               
PC100    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(6),0(R6)                                                    
         MVI   ELEM+1,RALQLENQ     ELEM LENGTH                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',REPFILE),(R3),ELEM,=C'ADD=CODE'                
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    PC100                                                            
*                                                                               
PC150    DS    0H                                                               
*&&DO                                                                           
         GOTO1 DATAMGR,DMCB,(0,ADDREC),REPFILE,KEY+28,NEWRATE,DMWORK            
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 UPDATE HEADER RECORD                         
         DC    H'0'                                                             
*&&                                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'RARTKEY),SVKEY                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,KEY                                                           
         OI    27(R6),X'80'        MARK KEY FOR DELETION                        
*&&DO                                                                           
         GOTO1 DATAMGR,DMCB,(0,DMWRT),REPDIR,KEY,KEY,0                          
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,OLDRATE,DMWORK            
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,OLDRATE                                                       
         OI    29(R6),X'80'        MARK RECORD FOR DELETION                     
*&&DO                                                                           
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,OLDRATE,DMWORK            
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 UPDATE HEADER RECORD                         
         DC    H'0'                                                             
*&&                                                                             
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
         B     PCSEQ                                                            
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT======>'                                           
*                                                                               
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
COUNT    DS    F                                                                
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
VHELLO   DS    F                                                                
*                                                                               
         DC    CL8'SAVEKEY'                                                     
SVKEY    DS    XL27                                                             
*                                                                               
         DC    CL8'OLDRATE'                                                     
OLDRATE  DS    XL2000                                                           
*                                                                               
         DC    CL8'NEWRATE'                                                     
NEWRATE  DS    XL2000                                                           
*                                                                               
         DC    CL8'ELEM'                                                        
ELEM     DS    XL50                                                             
ELCODE   DS    XL1                                                              
*                                                                               
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
       ++INCLUDE REGENARTE                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064REREP2402U05/01/02'                                      
         END                                                                    
