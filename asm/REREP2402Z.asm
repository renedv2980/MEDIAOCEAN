*          DATA SET REREP2402Z AT LEVEL 120 AS OF 05/01/02                      
*PHASE RE2402Z,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO CHECK EXISTING GAVAILS'                               
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
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   KEY,X'12'                                                        
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
*                                                                               
         CLI   RINVKSRC,C'M'       MARKET FACT?                                 
         BE    PCSEQ                                                            
         CLI   RINVKSRC,C'S'       STATION FACT?                                
         BE    PCSEQ                                                            
         CLI   RINVKSRC,0          INVENTORY HEADER?                            
         BNE   PCSEQ                                                            
*                                                                               
PC25     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,HDRIO,DMWORK              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC30     LA    R6,HDRIO                                                         
*                                                                               
         MVI   ELCODE,X'06'        GET AVAIL RATE ELEMENT IN HEADER             
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
*                                                                               
         CLI   1(R6),X'1B'         OLD STYLE X'06' ELEM?                        
         BNE   PCSEQ               NO - GET NEXT RECORD                         
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(2),HDRIO+10                                                    
         MVC   P+3(5),HDRIO+12      PRINT HEADER AND X'06' ELEMENTS             
         MVC   P+9(4),HDRIO+17                                                  
         GOTO1 HEXOUT,DMCB,HDRIO+21,P+19,3,=C'TOG'                              
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
PC40     CLI   1(R6),X'1B'         OLD STYLE X'06' ELEM?                        
         BNE   PCSEQ               NO - GET NEXT RECORD                         
*                                                                               
         MVC   P(2),2(R6)                                                       
         MVC   P+3(4),4(R6)        CODE                                         
         GOTO1 HEXOUT,DMCB,8(R6),P+9,2,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,10(R6),P+19,17,=C'TOG'                               
         GOTO1 REPORT                                                           
*                                                                               
PC100    BAS   RE,NEXTEL                                                        
         BE    PC40                                                             
*                                                                               
         B     PCSEQ               GET NEXT HEADER                              
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT ======>'                                          
*                                                                               
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
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
ELCODE   DS    X                                                                
*                                                                               
         DC    CL5'HDRIO'                                                       
HDRIO    DS    XL2000              HEADER RECORD TO CHANGE W/ NEW X'06'         
         LTORG                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVB                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120REREP2402Z05/01/02'                                      
         END                                                                    
