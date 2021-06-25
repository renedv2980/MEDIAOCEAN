*          DATA SET PPPUB09    AT LEVEL 020 AS OF 11/17/10                      
*PHASE T40609A                                                                  
*                                                                               
*     ****  CHANGE LOG  ****                                                    
*                                                                               
* SMYE  10/10   ADD "PUB LOCK" HANDLING                                         
*                                                                               
* SMYE 05/08/02 PUBEDIT NOW CORE-RESIDENT                                       
*                                                                               
* SMYE  11/01   ADD KILL DATE TO DATA IN PUB LIST FUNCTION                      
*                                                                               
* SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*               ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                  
*                                                                               
         TITLE 'T40609 - PUB FILE MAINT - LIST FUNCTION'                        
T40609   CSECT                                                                  
         NMOD1 0,T40609,RR=R9                                                   
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T406FFD,RA                                                       
         SPACE 2                                                                
*                                  SET KEY                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),PBLMED                                                    
         MVC   KEY+1(6),BPUB                                                    
         MVC   KEY+7(2),AGYALPHA                                                
         CLI   SAVSCRN,X'09'                                                    
         BE    FMT5                                                             
*                                  FETCH VIRGIN SCREEN                          
         LA    R6,PBLLAST                                                       
         GOTO1 VCALLOV,DMCB,(R6),X'D90406F9'                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SAVSCRN,X'09'                                                    
         B     FMT10                                                            
*                                                                               
FMT5     LA    R2,LSTLIN1H                                                      
         LA    R4,17                                                            
FMT8     XC    8(78,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         LA    R2,8+L'LSTLIN1(R2)                                               
         BCT   R4,FMT8                                                          
*                                                                               
FMT10    DS    0H                                                               
         LA    R2,LSTLIN1H                                                      
         MVI   DMOUTBTS,X'7D'      DONT LET KEN CATCH EOF                       
         EJECT                                                                  
         BAS   RE,HIGHPUB                                                       
         B     *+8                                                              
NXTREC   BAS   RE,SEQPUB                                                        
         TM    DMCB+8,X'80'        TEST EOF                                     
         BNZ   DONE                                                             
         MVC   BPUB,KEY+1                                                       
         CLC   KEY(1),PBLMED                                                    
         BNE   DONE                                                             
         CLI   KEY+9,X'81'                                                      
         BNE   NXTREC              ONLY BIG RECS                                
         CLC   KEY+7(2),AGYALPHA                                                
         BE    NXTREC1                                                          
         CLI   APROF,C'0'      NO STND RECS                                     
         BE    NXTREC                                                           
         CLC   KEY+7(2),=C'ZZ'     SRDS                                         
         BNE   NXTREC                                                           
NXTREC1  LA    R3,LSTLAST                                                       
         CR    R2,R3                                                            
         BL    GET                                                              
MORE     MVC   PBLMSG(L'MORMSG),MORMSG                                          
         FOUT  PBLMSGH                                                          
         LA    R2,PBLPUBH                                                       
         B     EXIT                                                             
         SPACE 2                                                                
GET      EQU   *                                                                
         BAS   RE,GETPUB                                                        
*                                  SEE IF FILTERING BY LOCK                     
         CLC   =C'LOCK=Y',PBLSCR       LOCKED FILTER ?                          
         BNE   GET10                                                            
         TM    PUBLOCSW,PUBLCKDQ   "LOCKED" ?                                   
         BNO   NXTREC              NO - SKIP                                    
         B     GET50                                                            
GET10    DS    0H                                                               
         CLC   =C'LOCK=N',PBLSCR       NOT LOCKED FILTER ?                      
         BNE   GET50                                                            
         TM    PUBLOCSW,PUBLCKDQ   "LOCKED" ?                                   
         BO    NXTREC              YES - SKIP                                   
*****    B     GET50                                                            
*                                                                               
GET50    DS    0H                                                               
         MVC   PUBKED,KEY+6         SET EDITION CODE IN PUBREC                  
*                                  IT COULD BE DIFFERENT THAN THE KEY           
         CLI   PUBKMED,C'N'                                                     
         BNE   MAG                                                              
         EJECT                                                                  
*                                  NEWSPAPER DISPLAY                            
         USING NDATAD,R2                                                        
NEWS     EQU   *                                                                
         IC    R7,APROF13                                                       
         GOTO1 VPUBEDIT,DMCB,((R7),PUBKPUB),NPUB                                
*                                                                               
         MVC   NNAME,PUBNAME                                                    
         MVC   NCITY,PUBCITY                                                    
         MVC   NSTATE,PUBSTATE                                                  
         OC    PUBKILL,PUBKILL     KILL DATE ?                                  
         BZ    NEWS1               NO                                           
         MVC   NKILL(3),=C'KD='                                                 
         GOTO1 VDATCON,DMCB,(3,PUBKILL),(5,NKILL+3)                             
NEWS1    CLC   PUBKAGY,=C'ZZ'                                                   
         BNE   *+10                                                             
         MVC   NSRDS,=C'*STND*'                                                 
*                                                                               
         TM    PUBLOCSW,PUBLCKDQ   "LOCKED" ?                                   
         BNO   *+10                                                             
         MVC   NSRDS,=C'  Y   '    YES                                          
*                                                                               
         OC    PUBZNAME,PUBZNAME                                                
         BZ    NXTFLD                                                           
         CLI   PUBZNAME,C' '                                                    
         BNE   *+14                                                             
         CLC   PUBZNAME+1(19),PUBZNAME                                          
         BE    NXTFLD                                                           
         LA    R3,8+L'LSTLIN1(R2)                                               
         LA    R4,LSTLAST                                                       
         CR    R3,R4                                                            
         BL    NEWS2                                                            
         XC    8(L'LSTLIN1,R2),8(R2)    DONT DISPLAY UNLESS CAN                 
         B     MORE                     GET ALL ON SCREEN                       
NEWS2    EQU   *                                                                
         FOUT  (R2)                                                             
         LR    R2,R3                                                            
         MVC   NZONE,PUBZNAME                                                   
         FOUT  (R2)                                                             
         B     NXTFLD                                                           
         EJECT                                                                  
*                                  MAGAZINE DISPLAY                             
         USING MDATAD,R2                                                        
MAG      EQU   *                                                                
         IC    R7,APROF13                                                       
         GOTO1 VPUBEDIT,DMCB,((R7),PUBKPUB),MPUB                                
*                                                                               
         MVC   MNAME,PUBNAME                                                    
         MVC   MZONE,PUBZNAME                                                   
         OC    PUBKILL,PUBKILL     KILL DATE ?                                  
         BZ    MAG1                NO                                           
         MVC   MKILL(3),=C'KD='                                                 
         GOTO1 VDATCON,DMCB,(3,PUBKILL),(5,MKILL+3)                             
MAG1     CLC   PUBKAGY,=C'ZZ'                                                   
         BNE   *+10                                                             
         MVC   MSRDS,=C'*STND*'                                                 
*                                                                               
         TM    PUBLOCSW,PUBLCKDQ   "LOCKED" ?                                   
         BNO   *+10                                                             
         MVC   MSRDS,=C' Y    '    YES                                          
*                                                                               
         FOUT  (R2)                                                             
         B     NXTFLD                                                           
         EJECT                                                                  
*                                  BUMP TO NEXT SCREEN LINE                     
NXTFLD   EQU   *                                                                
         LA    R2,8+L'LSTLIN1(R2)                                               
         B     NXTREC                                                           
         SPACE 2                                                                
*                                  NO MORE PUBS                                 
DONE     EQU   *                                                                
         XC    BPUB,BPUB                                                        
         MVC   PBLMSG(L'EOFMSG),EOFMSG                                          
         FOUT  PBLMSGH                                                          
         LA    R2,PBLMEDH                                                       
         B     EXIT                                                             
*                                                                               
*                                                                               
MORMSG   DC    C'TO CONTINUE LIST, TYPE ''NEXT'' AND ENTER'                     
EOFMSG   DC    C'DISPLAY COMPLETE'                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PUGENEROL                                                      
         EJECT                                                                  
       ++INCLUDE PUGENOLD                                                       
PUBIO    DS    4000C                                                            
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
         EJECT                                                                  
*        DSECT FOR NEWSPAPER LINE                                               
NDATAD   DSECT                                                                  
         DS    CL8                                                              
NDATA    DS    0CL78                                                            
NPUB     DS    CL17                                                             
         DS    CL1                                                              
NNAME    DS    CL20                                                             
         DS    CL1                                                              
NCITY    DS    CL16                                                             
         DS    CL1                                                              
NSTATE   DS    CL2                                                              
         DS    CL1                                                              
NKILL    DS    CL11                                                             
         DS    CL1                                                              
NSRDS    DS    CL6                                                              
         DS    CL1                                                              
         SPACE 2                                                                
*                           LINE 2                                              
         ORG   NDATAD                                                           
         DS    CL8                                                              
NDATA2   DS    0CL78                                                            
         DS    CL18                                                             
NZONE    DS    CL20                                                             
         DS    CL40                                                             
         SPACE 3                                                                
*        DSECT FOR MAG LINE                                                     
MDATAD   DSECT                                                                  
         DS    CL8                                                              
MDATA    DS    0CL78                                                            
MPUB     DS    CL17                                                             
         DS    CL1                                                              
MNAME    DS    CL20                                                             
         DS    CL1                                                              
MZONE    DS    CL20                                                             
         DS    CL1                                                              
MKILL    DS    CL11                                                             
         DS    CL1                                                              
MSRDS    DS    CL6                                                              
         EJECT                                                                  
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBF9D                                                       
         ORG   T406FFD                                                          
         SPACE 2                                                                
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    X                                                                
APROF13  DS    X                                                                
BCODE    DS    CL3                                                              
         SPACE 3                                                                
       ++INCLUDE FLDIND                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020PPPUB09   11/17/10'                                      
         END                                                                    
