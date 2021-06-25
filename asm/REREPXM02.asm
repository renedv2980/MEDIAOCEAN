*          DATA SET REREPXM02  AT LEVEL 197 AS OF 10/05/06                      
*PHASE REXM02A,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'MODULE TO CREATE ROUTING TABLE FOR DARE PLUS DB2'               
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREPXM02 (REXM02) --- XML ROUTING TABLE EXTRACT         *             
*                                                                 *             
*******************************************************************             
*                                                                               
REXM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REXM02,R9,RR=R5                                              
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
         L     RF,=V(RECUP)                                                     
         L     RE,RELO                                                          
         AR    RF,RE                                                            
         ST    RF,VRECUP                                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         OPEN  (FILOUTA,(OUTPUT))                                               
*                                                                               
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,CTIKTYPQ                                                     
         MVC   KEY+23(2),=X'FFFF'                                               
*        MVI   KEY+15,C'X'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,IOAREA                    
         CLI   DMCB+8,0                                                         
         BNE   PCEXIT                                                           
         B     PC10                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         XC    P,P                                                              
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'CTFILE',KEY,IOAREA                    
         CLI   IOAREA,C'I'                                                      
         BNE   PCEXIT                                                           
         CLI   IOAREA+15,C'A'                                                   
         BL    PCEXIT                                                           
         CLI   IOAREA+15,C'9'                                                   
         BH    PCEXIT                                                           
*        MVC   P(80),IOAREA                                                     
*        GOTO1 REPORT                                                           
*                                                                               
PC10     DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'33'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
         USING CTUSAD,R6                                                        
         CLI   CTUSADPI,C'M'       MUST BE MQ                                   
         BNE   PCSEQ                                                            
         TM    CTUSADFL,CTUSADFX   MUST BE XML USER                             
         BZ    PCSEQ                                                            
*                                                                               
         XC    REC-4(4),REC-4                                                   
         MVI   REC-3,XMLLENQ+4     INSERT LENGTH FOR PUT                        
*                                                                               
         MVC   REC(XMLLENQ),SPACES                                              
*                                                                               
         MVC   XMLRTYPE,=C'05599'                                               
*                                                                               
         MVI   XMLACT,C'L'                                                      
         GOTO1 =V(DATCON),DMCB,(5,0),(10,XMLDATE)                               
*                                                                               
         THMS  DDSTIME=NO                                                       
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
*        ST    R1,DUB+4            DDS TIME                                     
*        AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,HHMMSS                                                      
         GOTO1 =V(HEXOUT),DMCB,HHMMSS,XMLTIME,1,=C'TOG'                         
         MVI   XMLTIME+2,C':'                                                   
         GOTO1 =V(HEXOUT),DMCB,HHMMSS+1,XMLTIME+3,1,=C'TOG'                     
         MVI   XMLTIME+5,C':'                                                   
         GOTO1 =V(HEXOUT),DMCB,HHMMSS+2,XMLTIME+6,1,=C'TOG'                     
*                                                                               
         MVI   XMLSC1,X'5E'        SEMI-COLON                                   
         MVI   XMLSC2,X'5E'        SEMI-COLON                                   
         MVI   XMLSC3,X'5E'        SEMI-COLON                                   
         MVI   XMLSC4,X'5E'        SEMI-COLON                                   
         MVI   XMLSC5,X'5E'        SEMI-COLON                                   
         MVI   XMLSC6,X'5E'        SEMI-COLON                                   
         MVI   XMLSC7,X'5E'        SEMI-COLON                                   
*                                                                               
         MVC   XMLRTCDE,CTUSADRC                                                
         MVC   P+69(5),XMLRTCDE                                                 
*                                                                               
         LA    R4,NSIOFF                                                        
PC20     CLC   3(2,R4),CTUSADRC+3                                               
         BE    PC30                                                             
         LA    R4,5(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    PCSEQ                                                            
         B     PC20                                                             
*                                                                               
PC30     DS    0H                                                               
*                                                                               
         MVC   XMLMKTCD,0(R4)                                                   
         MVC   P+66(3),XMLMKTCD                                                 
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
         USING CTDSTD,R6                                                        
         MVC   XMLAGYNM(33),CTDSTNAM                                            
         MVC   P(33),XMLAGYNM                                                   
*                                                                               
* AGENCY NAME MAY BE MORE THAN 33 CHARACTERS. OVERFLOW CHARACTERS ARE           
* CONTINUED IN THE ORIGIN NAME FIELD                                            
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'36'                                                     
         BAS   RE,GETEL                                                         
         BNE   PC33                                                             
         USING CTORGD,R6                                                        
         MVC   XMLAGYNM+33(33),CTORGNAM                                         
         OC    XMLAGYNM+33(33),SPACES                                           
         MVC   P+33(33),XMLAGYNM+33                                             
*                                                                               
* REPLACE EXTRA TRAILING BLANKS WITH SEMICOLON                                  
* DUPLCIATE SEMICOLONS WILL BE STRIPPED IN THE END                              
*                                                                               
PC33     DS    0H                                                               
         LA    R2,XMLAGYNM+L'XMLAGYNM-1                                         
         LA    R3,L'XMLAGYNM                                                    
PC35     CLC   0(2,R2),=X'405E'                                                 
         BNE   PC40                                                             
         MVI   0(R2),X'5E'                                                      
         BCTR  R2,0                                                             
         BCT   R3,PC35                                                          
*                                                                               
PC40     DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING CTIREC,R6                                                        
         MVC   XMLSENDR,CTIKID                                                  
         MVC   P+74(10),XMLSENDR                                                
*                                                                               
* REPLACE EXTRA TRAILING BLANKS WITH SEMICOLON                                  
* DUPLCIATE SEMICOLONS WILL BE STRIPPED IN THE END                              
*                                                                               
         LA    R2,XMLSENDR+L'XMLSENDR-1                                         
         LA    R3,L'XMLSENDR                                                    
PC45     CLC   0(2,R2),=X'405E'                                                 
         BNE   PC48                                                             
         MVI   0(R2),X'5E'                                                      
         BCTR  R2,0                                                             
         BCT   R3,PC45                                                          
*                                                                               
* CHECK FOR DUPLICATE SEMICOLONS AND REMOVE EXTRAS                              
*                                                                               
PC48     DS    0H                                                               
         LA    R2,REC                                                           
         LA    R3,XMLOUT                                                        
         MVC   XMLOUT,SPACES                                                    
         LA    R4,XMLLENQ                                                       
PC50     DS    0H                                                               
         CLC   0(2,R2),=X'5E5E'                                                 
         BE    PC60                                                             
         MVC   0(1,R3),0(R2)                                                    
*                                                                               
         LA    R3,1(R3)                                                         
*                                                                               
PC60     DS    0H                                                               
         LA    R2,1(R2)                                                         
         BCT   R4,PC50                                                          
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(XMLLENQ),XMLOUT                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   REC(XMLLENQ),XMLOUT                                              
         BAS   RE,PUTRECS                                                       
*                                                                               
         B     PCSEQ                                                            
*                                                                               
PCEXIT   DS    0H                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
*                                                                               
******************************************************************              
*  PUTRECS: GENERATE OUTFILE ENTRIES TO TAPE                     *              
******************************************************************              
PUTRECS  NTR1                                                                   
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         B     EXIT                                                             
*                                                                               
         GETEL R6,28,ELCODE                                                     
*                                                                               
       ++INCLUDE REGENNOF                                                       
*                                                                               
VHELLO   DS    A                                                                
VRECUP   DS    A                                                                
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(REXM02,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
*                                                                               
WRTOFILE DS    XL1                 WRITE TO FILE (Y OR N)                       
*                                                                               
MYFLAG   DS    XL1                                                              
PRNTHDR  EQU   X'01'               PRINT HEADER INFO                            
*                                                                               
ZCOUNT   DS    F                   # OF RDETAIL DELETED                         
RCOUNT   DS    F                   # OF RATE CARDS DELETED                      
ELEM     DS    XL50                                                             
ELCODE   DS    X                                                                
HHMMSS   DS    XL3                                                              
*                                                                               
*                                                                               
* 8K BLOCK SIZE, 9 TRACK, 1600 BPI TAPE ??????                                  
*                                                                               
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,LRECL=118,    X        
               BLKSIZE=8192,BUFNO=2                                             
*                                                                               
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL(XMLLENQ)                                                      
         ORG   REC                                                              
XMLRTYPE DS    CL5                                                              
XMLSC1   DS    CL1                                                              
XMLACT   DS    CL1                 SHOULD ONLY BE C'L'                          
XMLSC2   DS    CL1                                                              
XMLDATE  DS    CL8                 FORMAT DD/MM/YY                              
         DS    CL1                                                              
XMLTIME  DS    CL8                 FORMAT HH:MM:SS                              
XMLSC3   DS    CL1                                                              
XMLAGYNM DS    CL66                                                             
XMLSC4   DS    CL1                                                              
XMLMKTCD DS    CL3                                                              
XMLSC5   DS    CL1                                                              
XMLRTCDE DS    CL5                                                              
XMLSC6   DS    CL1                                                              
XMLSENDR DS    CL10                                                             
XMLSC7   DS    CL1                                                              
XMLLENQ  EQU   *-XMLRTYPE                                                       
*                                                                               
XMLOUT   DS    XL(XMLLENQ)                                                      
*                                                                               
IOAREA   DS    XL2000              HEADER RECORD TO CHANGE W/ NEW X'06'         
*                                                                               
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
       ++INCLUDE RCARDDELD                                                      
*                                                                               
       ++INCLUDE REGENARTE                                                      
*                                                                               
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'197REREPXM02 10/05/06'                                      
         END                                                                    
