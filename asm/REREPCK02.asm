*          DATA SET REREPCK02  AT LEVEL 170 AS OF 02/21/96                      
*PHASE RECK02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
         TITLE 'REREPCK02A (RECK02A) --- EDI 1 SHOT FIX'                        
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPCK02A -- EDI 1 SHOT FIX                              *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* FEB21/96 ABBEY --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RECK02   CSECT                                                                  
         NMOD1 0,**RECK**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
MAIN0040 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'51'           INSERT KEY TYPE: DARE POINTERS               
         MVC   KEY+07(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                GET FIRST RECORD                             
         B     MAIN0080                                                         
MAIN0060 EQU   *                                                                
         GOTO1 SEQ                                                              
MAIN0080 EQU   *                                                                
         CLC   KEY(09),KEYSAVE     SAME TYPE/REP/ACT?                           
         BNE   MAIN0100            NO  - FINISHED                               
*                                                                               
         MVC   DAREKEY,KEY         SAVE FOR RESTART                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,             X        
               RECORD,(0,IOWORK)                                                
         TM    DMCB+8,X'10'        REC NOT FOUN                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,RECORD                                                        
         USING RDARREC,R2                                                       
         MVC   SVREFNO,RDARAGAD+5                                               
         XC    WORK(20),WORK       CLEAR KEY AREA                               
         MVC   WORK(5),RDARKAGY    SAVE AGENCY/AGYOFF ROUTING                   
         GOTO1 CONTPROC,DMCB,(RC)                                               
*                                  PROCESS CONTRACT AND BUYS                    
         OC    WORK+5(6),WORK+5    CONTRACT FOUND?                              
         BNZ   MAIN0090            YES                                          
         MVC   P+1(19),=C'CONTRACT NOT FOUND:'                                  
         GOTO1 HEXOUT,DMCB,RDARREP#,P+22,4,=C'TOG'                              
         GOTO1 REPORT                                                           
*                                                                               
MAIN0090 EQU   *                                                                
         MVC   KEY(27),DAREKEY     RESET LAST DARE KEY                          
         GOTO1 HIGH                RESET TO LAST KEY                            
         B     MAIN0060            GO BACK FOR NEXT DARE RECORD                 
*                                                                               
MAIN0100 EQU   *                                                                
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CONTPROC:  FOR EACH CONTRACT, CHECK FOR THE 1D ELEM WHICH     *              
*     SHOULD ALWAYS BE THERE. PRINT IT OUT. CORRECT IT AND       *              
*     PRINT IT OUT AGAIN.                                        *              
*                                                                *              
******************************************************************              
*                                                                               
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*                                                                               
         XC    KEY,KEY             CLEAR KEY FOR BUYLINE READ                   
         MVI   KEY,X'8C'           INSERT RECORD TYPE                           
         MVC   KEY+21(2),RCREPFL   INSERT SELTEL REP CODE                       
         MVC   KEY+23(4),RDARREP#                                               
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),KEY+23(4)                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+23(4),WORK+15   INSERT THE COMP'D KEY                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    CPRO0020            YES                                          
         MVC   P+1(19),=C'CONTRACT NOT FOUND:'                                  
         MVC   P+15(20),KEYSAVE                                                 
         MVC   P+40(7),=C'REF NO:'                                              
         MVC   P+50(25),SVREFNO                                                 
         GOTO1 REPORT                                                           
         B     CPRO2000            EXIT                                         
CPRO0020 EQU   *                                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,         X        
               RECORD2,(0,IOWORK)                                               
         TM    DMCB+8,X'10'        REC NOT FOUN                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK+5(6),RCONKAGY  SAVE AGENCY/AGENCY OFFICE                    
*                                                                               
         MVI   ELCODE,X'1D'        DARE AGENCY ORDER ELEM                       
         LA    R6,RECORD2                                                       
         LR    R5,R6                                                            
         BAS   RE,GETEL                                                         
         BE    CPRO0030                                                         
         MVC   P+1(31),=C'1D ELEM NOT FOUND FOR CONTRACT:'                      
         MVC   P+40(27),RECORD2                                                 
         GOTO1 REPORT                                                           
         B     CPRO2000                                                         
*                                                                               
CPRO0030 EQU   *                                                                
*                                                                               
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'02'      KATZ ONE SHOT?                               
         BZ    CPRO2000                                                         
         MVC   P+1(7),=C'REF NO:'                                               
         MVC   P+10(25),SVREFNO                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'CONTRACT RECORD:'                                     
         GOTO1 REPORT                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',27,=C'1D'                  
         GOTO1 REPORT                                                           
         MVC   P+1(15),=C'OLD 1D ELEMENT:'                                      
         GOTO1 REPORT                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R6)),(R6),C'DUMP',19,=C'1D'                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   WORK2(8),SVREFNO+2                                               
         OC    WORK2(8),FOXZEROS      DEAL WITH LETTERS                         
         PACK  DUB+3(5),WORK2(8)                                                
         XC    WORK2,WORK2                                                      
         MVO   WORK2+10(5),DUB+3(5)                                             
         MVC   WORK2(5),WORK2+10                                                
         MVC   RCONDRLK,WORK2                                                   
*                                                                               
         DROP  R6                                                               
         CLI   QOPTION1,C'U'       UPDATE?                                      
         BNE   CPRO0035                                                         
         GOTO1 DATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28,         X        
               RECORD2,IOWORK                                                   
*                                                                               
CPRO0035 MVC   P+1(15),=C'NEW 1D ELEMENT:'                                      
         GOTO1 REPORT                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R6)),(R6),C'DUMP',19,=C'1D'                  
         GOTO1 REPORT                                                           
*                                                                               
CPRO2000 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
INITIAL  NTR1                                                                   
         SPACE 1                                                                
*                                  GET 100K STORAGE SPACE                       
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',100000,100000                                   
*                                  GET 100K STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         L     RF,ARECNEXT                                                      
         A     RF,=F'40000'        TAPE BUFFER AREA:                            
*                                                                               
         LA    RF,RECORD                                                        
         ST    RF,AIOAREA          SET A(IOAREA) TO FIRST                       
         ST    RF,AIOAREA1         SAVE A(RECORD)                               
         LA    RF,RECORD2                                                       
         ST    RF,AIOAREA2         SAVE A(RECORD2)                              
         LA    RF,RECORD3                                                       
         ST    RF,AIOAREA3         SAVE A(RECORD3)                              
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
       ++INCLUDE RGENIO                                                         
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
ARECNEXT DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
AIOAREA  DS    F                                                                
AIOAREA1 DS    F                                                                
AIOAREA2 DS    F                                                                
AIOAREA3 DS    F                                                                
FOXZEROS DC    C'0000000000000000'                                              
COMMAND  DS    CL8                                                              
SVREFNO  DS    CL25                                                             
WORK2    DS    CL20                                                             
*                                                                               
DAREKEY  DS    CL27                DARE KEY SAVE AREA                           
*                                                                               
ELCODE   DS    CL1                                                              
IOWORK   DS    12D                                                              
*                                                                               
*                                                                               
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=FB,MACRF=PM,              X        
               LRECL=529,BLKSIZE=21160,BUFNO=2                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE DMPRTQL                                                              
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL1024                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
RECORD2  DS    CL1024                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENAGY2                                                      
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
         ORG                                                                    
RECORD3  DS    CL1024                                                           
         ORG   RECORD3                                                          
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
         ORG                                                                    
EDIREC11 DS    529C                                                             
EDIREC12 DS    529C                                                             
EDIREC13 DS    529C                                                             
EDIREC14 DS    529C                                                             
EDIREC99 DS    529C                                                             
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DRKZEDIAKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'170REREPCK02 02/21/96'                                      
         END                                                                    
