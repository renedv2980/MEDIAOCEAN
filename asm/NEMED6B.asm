*          DATA SET NEMED6B    AT LEVEL 088 AS OF 10/26/12                      
*PHASE T31E6BC,*                                                                
*INCLUDE NETNET                                                                 
*INCLUDE DDUCOM                                                                 
         TITLE 'T31E6B-SCJ NETWORK INTERFACE TAPE'                              
T31E6B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SCJP**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          R7-ANETWS2/WORKING STORAGE                   
         USING WORKD,R7                                                         
         LA    R5,1(RB)           R5=SECOND BASE REG                            
         LA    R5,4095(R5)                                                      
         USING T31E6B,RB,R5                                                     
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANTWKTP                                                       
         LA    R1,NETBUFF                                                       
         ST    R1,NBANBUFF                                                      
*                                                                               
         EJECT                                                                  
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
* GETS UNIT RECS/WRITES TO TAPE/PRINT LINE                                      
*                                                                               
*****************************************                                       
LR       DS    0H                                                               
         XC    RECTOT,RECTOT                                                    
         ZAP   NETTOTS,=P'0'                                                    
         ZAP   GRSTOTS,=P'0'                                                    
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
         B     LR5                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,67,A),FORMAT=BI,WORK=1'                      
* KEY LENGTH=DETAIL REC                                                         
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=150'                                   
*                                                                               
LR5      DS    0H                                                               
         CLI   SPLFILE,C'Y'                                                     
         BNE   LR7                                                              
*                                                                               
         LA    R2,MQFILENM                                                      
         USING FILNAMD,R2                                                       
         MVC   FNPFIX,=C'SFTPDISK.PROD.'                                        
         BRAS  RE,TESTRUN                                                       
         BNE   *+10                                                             
         MVC   FNPFIX+9(4),=C'TEST'                                             
*                                                                               
*        MVC   FNPRG(FILNAMX-FNPRG),=CL30'EST.NE.MIND.DYYMMDD.THHMMSST'         
         MVC   FNPRG(FILNAMX-FNPRG),=CL30'BUY.NE.SJSJ.DYYMMDD.THHMMSST'         
         L     RF,TWAMASTC                                                      
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   FNAGYLBL,MCAGYCOD-MCEXTRA(RF)                                    
         GOTO1 DATCON,DMCB,(5,WORK),(X'20',FNYYMMDD)                            
         TIME  DEC                     R0=HHMMSSHH                              
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,FNTIME,4                                        
         MVI   FNTIME+7,X'40'                                                   
*                                                                               
* DUNALLOC FOR DATASET                                                          
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         LHI   R0,3                NUMBER OF RETIRES WITH DYNALLOC              
         XC    DMCB(16),DMCB                                                    
DYNALLIT GOTO1 (RF),DMCB,(X'80',=CL8'OUTP'),                           X        
               (X'87',=XL6'000003000005'),                             X        
               (X'C0',MQFILENM)                                                 
         OC    DMCB+12(4),DMCB+12                                               
         BZ    *+10                                                             
         BCT   R0,DYNALLIT         ERROR,TRY AGAIN                              
         DCHO                                                                   
*                                                                               
         LA    R2,OUTP                                                          
         OPEN  ((R2),(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR7      DS    0H                                                               
*                                                                               
*                                                                               
         MVI   NBDATA,C'U'         SET UP FOR UNIT RECS                         
         MVI   NBSEQ,C'D'          SET UP FOR DATE SEQ                          
         OI    NBSPLOPT,X'80'                                                   
***      MVI   NBSELUOP,C'A'       ACTUAL SCHEDULE                              
         MVI   NBESTOPT,0                                                       
         MVI   NBACTOPT,0                                                       
*                                                                               
LR10     NETGO NSNETIO,DMCB,NETBLOCK     GET UNIT RECS                          
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NBMODE,NBVALDAT                                                  
         BNE   *+8                                                              
         BAS   RE,CALENDAR         MOS CALENDAR                                 
         CLI   NBMODE,NBPROCUN                                                  
         BE    LR20                                                             
         CLI   NBMODE,NBREQLST     LAST RECORD                                  
         BE    LR30                                                             
         B     LR10                                                             
*                                                                               
LR20     DS    0H                                                               
         CLC   =X'000000',NBSPLPR3    SKIP UNALLOCATED UNITS                    
         BE    LR10                                                             
         BAS   RE,DODETAIL         SET UP DETAIL REC                            
         CLI   BYTE,C'N'                                                        
         BE    LR10                                                             
         BAS   RE,PUTSORT          PUT IT TO SORT                               
         B     LR10                GET NXT UNIT REC                             
                                                                                
*                                                                               
*   GET RECORDS FROM SORTER                                                     
LR30     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    LR40                EOF SORTER                                   
         MVC   NEWREC,0(R3)                                                     
         LA    R3,NEWREC                                                        
         USING PHDETAIL,R3                                                      
*                                                                               
*                                                                               
* CONVERT BINARY MOS TO MMMYYYY                                                 
         GOTO1 DATCON,DMCB,(2,PHESTDCR+3),(21,WORK) MMMDD/YYYY                  
         MVC   WORK+3(4),WORK+6                                                 
         MVC   PHESTDCR+3(7),WORK  MMMYYYY                                      
*                                                                               
         CLI   FRST,0           ** FIRST REC**                                  
         BNE   *+12                NO                                           
         BAS   RE,DOHEAD           YES                                          
         B     LR30                GET NEXT SORTER REC                          
*                                                                               
         CLC   OLDREC(PHDETKLN),NEWREC   CLI/PROD/EST BREAK?                    
         BE    LR33                                                             
         B     LR32                                                             
                                                                                
*                                                                               
* KEYS DO NOT MATCH - CLI/PROD/EST BREAK- PREPARE OLD REC FOR OUTPUT            
LR32     LA    R2,OLDREC                                                        
D        USING PHDETAIL,R2                                                      
*                                                                               
         L     R1,RECTOT           UPDATE TOTAL REC COUNT                       
         LA    R1,1(R1)                                                         
         ST    R1,RECTOT                                                        
*                                                                               
         AP    NETTOTS,D.PHNETASS(8)  ADD BREAK TOTALS TO REPORT TOTALS         
         AP    GRSTOTS,D.PHGRSASS(8)                                            
                                                                                
* EDIT REC DOLLARS INTO SPECIFIED TAPE FORMAT                                   
         EDIT  (P8,D.PHNETASS),(15,D.PHNETASS+1),2                              
         EDIT  (P8,D.PHGRSASS),(15,D.PHGRSASS+1),2                              
         MVI   D.PHNETASS,C'+'                                                  
         MVI   D.PHGRSASS,C'+'                                                  
         LA    R1,D.PHNETASS+1                                                  
LR32A    CLI   0(R1),X'40'                                                      
         BH    LR32B                                                            
         MVI   0(R1),X'F0'                                                      
         LA    R1,1(R1)                                                         
         B     LR32A                                                            
LR32B    LA    R1,D.PHGRSASS+1                                                  
         CLI   0(R1),X'40'                                                      
         BH    LR32C                                                            
         MVI   0(R1),X'F0'                                                      
         LA    R1,1(R1)                                                         
         B     LR32B+4                                                          
LR32C    MVC   D.PHNETAS2,D.PHNETASS                                            
         B     LR32D                                                            
*                                                                               
LR32D    MVC   OUTREC,OLDREC       MOVE OLD REC OUT FOR TAPE/PRINT              
         MVC   OLDREC,NEWREC       REPLACE OLDREC WITH NEW SORTER REC           
         BAS   RE,SENDIT                                                        
         CLI   NEWREC,0            NO MORE DETAILS?                             
         BE    LR45                                                             
         B     LR30                                                             
         DROP  D                                                                
****************************************************************                
*  KEYS MATCH - ROLL UP TOTALS TO NEWREC AND SET NEWREC AS OLDREC               
*                                                                               
LR33     LA    R2,OLDREC                 ROLL UP TOTALS                         
D        USING PHDETAIL,R2                                                      
         AP    PHNETASS(8),D.PHNETASS(8) ADD TO NEWREC FIELDS                   
         AP    PHGRSASS(8),D.PHGRSASS(8)                                        
         MVC   OLDREC,NEWREC             REPLACE OLD REC WITH NEW               
         B     LR30                      GET NEXT REC FROM SORT                 
         DROP  D                                                                
                                                                                
*************************************************************                   
LR40     DS    0H                                                               
         CLI   OLDREC,0           IS THERE ANY DATA                             
         BE    LRX                                                              
         XC    NEWREC,NEWREC                                                    
         B     LR32                PROCESS THE OLDREC                           
LR45     DS    0H                                                               
         BAS   RE,DOFOOTER                                                      
         CLI   SPLFILE,C'Y'                                                     
         BNE   LRX                                                              
*                                                                               
         BAS   RE,MQMSSG                                                        
*                                                                               
         BAS   RE,LASTRUN                                                       
LRX      B     EXIT                                                             
         SPACE                                                                  
*                                                                               
*                                                                               
LASTRUN  DS    0H                                                               
         LA    R2,OUTP                                                          
         CLOSE ((R2),)                                                          
LASTRX   B     EXIT                                                             
         EJECT                                                                  
********************************************                                    
* ROUTINE SETS DATA TO REQUEST RECORD                                           
*  AND WRITES REQ REC TO REPORT AND TAPE                                        
*                                                                               
DODETAIL NTR1                                                                   
         MVI   BYTE,0              CLEAR FLAG                                   
         LA    R3,NEWREC                                                        
         USING PHEADER,R3                                                       
         MVI   0(R3),X'40'                                                      
         MVC   1(L'PHEAD-1,R3),0(R3)            CLEAR TO SPACES                 
         MVC   PHDETAIL,=CL10'DETAIL'                                           
         BAS   RE,GETUCOM          RETURNS URN IN WORK                          
         MVC   PHURN,WORK          *URN                                         
         CLC   PHURN,=8X'40'       SKIP IF EMPTY                                
         BH    *+8                                                              
         MVI   BYTE,C'N'                                                        
         MVC   PHPROD,NBSPLPR3                     *PRODUCT                     
         MVC   PHEST,=CL8' '                    *ESTIMATE                       
         EDIT  (B1,NBACTEST),(3,PHEST)             *ESTIMATE                    
         CLI   PHEST,X'40'      SET LEADING ZEROS TO EST NUMBER                 
         BH    DODET20                                                          
         MVI   PHEST,C'0'                                                       
         CLI   PHEST+1,X'40'                                                    
         BH    DODET20                                                          
         MVI   PHEST+1,C'0'                                                     
*                                                                               
DODET20  BAS   RE,GETEST       *RETURNS EST STRT DAT IN WORK YYYYMMDD           
         MVC   PHESTDAT,WORK                                                    
*                                                                               
*                                    *ESTIMATE DESCRIPTION                      
         MVC   PHESTDCR(3),NBSPLPR3   PROD                                      
         BAS   RE,GETUNMOS            RETURNS UNIT MOS IN WORK MMMYYYY          
         MVC   PHESTDCR+3(2),HALF       BINARY MOS FOR SORT                     
***      MVC   PHESTDCR+3(7),WORK                                               
         MVI   PHESTDCR+10,C'N'                                                 
         MVC   PHESTDCR+11(1),NBEFFMED                                          
*                                                                               
         L     RF,=V(NETNET)                                                    
         GOTO1 (RF),DMCB,(NBRTTYPE,NBASSIGN),GROSSNET                           
         ICM   R0,15,GROSSNET                                                   
         CVD   R0,PHGRSASS         STORE GROSS PACKED                           
         ICM   R0,15,GROSSNET+4                                                 
         CVD   R0,PHNETASS         STORE NET PACKED                             
**       EDIT  (B4,GROSSNET+4),(15,PHNETASS+1),2    NET                         
**       EDIT  (B4,GROSSNET),(15,PHGRSASS+1),2      GROSS                       
**       MVI   PHNETASS,C'+'                                                    
**       MVI   PHGRSASS,C'+'                                                    
**       MVC   PHNETAS2(8),GROSSNET             SAVE BINARY                     
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
***************************************                                         
* ROUTINE PUTS RECWORK TO SORTER                                                
*   AND CLEARS RECWORK                                                          
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',NEWREC                                       
         B     EXIT                                                             
         SPACE 3                                                                
*******************************                                                 
*                                                                               
SENDIT   NTR1                                                                   
*                                                                               
SEND10   DS    0H                                                               
         MVI   P,X'40'                                                          
         MVC   P+1(L'P-1),P                                                     
         MVC   P2,P                                                             
         CLC   =C'HEADER',OUTREC                                                
         BNE   *+14                                                             
         MVC   P(PHHEADLN),OUTREC                                               
         B     SEND20                                                           
*                                                                               
         CLC   =C'FOOTER',OUTREC                                                
         BNE   *+14                                                             
         MVC   P(PHFOOTLN),OUTREC                                               
         B     SEND20                                                           
*                                                                               
         CLC   =C'DETAIL',OUTREC                                                
         BE    *+6                                                              
         DC    H'0'                ???                                          
         MVC   P(PHDETKLN),OUTREC                                               
         MVC   P+PHDETKLN(48),OUTREC+PHDETKLN                                   
*                                                                               
SEND20   CLI   SPLFILE,C'Y'                                                     
         BNE   SEND22                                                           
         MVC   OUTREC,P                                                         
         LA    R3,OUTREC                                                        
         LA    R1,OUTP                                                          
         PUT   (R1),(R3)                 WRITE TAPE                             
         B     EXIT                                                             
SEND22   GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
GETEST   NTR1                                                                   
         LA    R3,KEY                                                           
         USING EKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NBACTEST                                                 
*                                                                               
         LA    R1,ESTABLE                                                       
GETEST10 CLI   0(R1),0                                                          
         BE    GETEST25                                                         
         CLC   EKEYCLT(6),0(R1)    CLT/PROD/EST                                 
         BE    GETEST20                                                         
         LA    R1,14(R1)           BUMP ESTABLE                                 
         B     GETEST10                                                         
GETEST20 MVC   WORK(8),6(R1)                                                    
         B     GETESTX                                                          
*                                                                               
GETEST25 NETGO NVSETSPT,DMCB                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         LA    R1,MYIO                                                          
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING ESTHDR,R3                                                        
         GOTO1 DATCON,DMCB,ESTART,(20,WORK)      YYYYMMDD IN WORK               
*                                                                               
         LA    R1,ESTABLE          ADD DATE TO TABLE                            
GETEST40 CLI   0(R1),X'FF'         EOF?                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),0             EMPTY SLOT?                                  
         BE    *+12                                                             
         LA    R1,14(R1)           BUMP ESTABLE                                 
         B     GETEST40                                                         
         MVC   6(8,R1),WORK        ADD DATE TO TABLE                            
*                                                                               
         NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME   CLEAR FILENAME                               
GETESTX  B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
GETUNMOS NTR1                                                                   
         LA    R3,DATEAREA                                                      
GTMOS10  CLC   NBACTDAT,2(R3)                                                   
         BNH   GTMOS20                                                          
         LA    R3,4(R3)            BUMP MONTH STRT/END DATE                     
         CLI   0(R3),0                                                          
         BNE   GTMOS10                                                          
         DC    H'0'                ???                                          
GTMOS20  MVC   HALF,2(R3)                                                       
         GOTO1 DATCON,DMCB,(2,HALF),(0,WORK)   YYMMDD IN WORK                   
         L     R2,=F'-10'                     MINUS 10 MONTH END                
         GOTO1 ADDAY,DMCB,(0,WORK),(0,WORK),(R2)                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,HALF)   BINARY MOS                       
         GOTO1 DATCON,DMCB,(0,WORK),(21,WORK)  MMMDD/YYYY                       
         MVC   WORK+3(4),WORK+6                 -> MMMYYYY AT WORK              
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
GETUCOM  NTR1                                                                   
         XC    WORK,WORK           RETURN FIELD DATA IN WORK                    
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING DDUCOMD,RF                                                       
         MVC   UCACOMF,ACOMFACS                                                 
         MVI   UCSYS,C'N'                                                       
         MVC   UCSAM,NBACTAM                                                    
         MVC   UCSCLT,NBACTCLI                                                  
         MVC   UCPRD,NBSPLPR3      SET PRODUCT                                  
         MVC   UCSEST,NBACTEST     SET ESTIMATE                                 
         OI    UCOPT,UCOEST        RETURN EST UCOM                              
         MVC   DUB(7),UCSCLT       SAVE KEY FOR TABLE                           
*                                                                               
         L     R1,=A(UCOMTBL)      IS IT IN TABLE?                              
GETUCM05 CLI   0(R1),0                                                          
         BE    GETUCM10                                                         
         CLC   UCSCLT(7),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,15(R1)                                                        
         B     GETUCM05                                                         
         MVC   WORK(8),7(R1)                                                    
         B     GETUCOMX                                                         
*                                                                               
GETUCM10 GOTO1 =V(DDUCOM),ELEM                                                  
         LA    RF,ELEM                                                          
         CLI   UCERROR,0           ERROR?                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UCDATA,UCDNOEST     NO EST LEVEL(PTOBLEM?)                       
         BO    GETUCOMX                                                         
**       ZIC   R1,FULL             GET P1,P2 ETC POSITION                       
**       BCTR  R1,0                -1 FOR INDEX                                 
**       MHI   R1,32               X LENGTH OF DATA FIELDS                      
         L     RE,UCEDATA          START OF DATA FIELDS                         
**       AR    RE,R1               RE NOW POINTS TO REQUIRED DATA               
**       ZIC   R1,FULL+1           GET OUTPUT LENGTH                            
**       BCTR  R1,0                -1 FOR MVC                                   
**       EX    R1,*+8                                                           
**       B     *+10                                                             
**       MVC   WORK(0),0(RE)                                                    
         MVC   WORK(8),0(RE)                                                    
*                                                                               
         L     R1,=A(UCOMTBL)     ADD TO TABLE?                                 
GETUCM15 CLI   0(R1),0                                                          
         BE    GETUCM20                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'               NEED LARGER TABLE                             
         LA    R1,15(R1)                                                        
         B     GETUCM15                                                         
GETUCM20 MVC   0(7,R1),DUB         SET  KEY TO TABLE                            
         MVC   7(8,R1),WORK        SET URN NUMBER                               
         B     GETUCOMX                                                         
*                                                                               
GETUCOMX XIT1                                                                   
         EJECT                                                                  
*                                                                               
OUTP     DCB   DDNAME=OUTP,                                            X        
               DSORG=PS,                                               X        
               LRECL=132,                                              X        
               BLKSIZE=3300,                                           X        
               RECFM=FB,                                               X        
               MACRF=PM                                                         
*                                                                               
         GETEL (R2),DATADISP,ELCODE                                             
*                                                                               
CALENDAR NTR1                                                                   
         LA    R1,25               MONTHS                                       
         ST    R1,NMONTHS                                                       
         XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'M'                                                     
         MVI   NBPEROVR,C'S'       SPECIAL CALENDAR                             
         NETGO NVWKLST,DMCB,NMONTHS,DATEAREA,PERTYPE                            
*                                                                               
*  GET MOS TABLE FOR REPORT REQUEST DATE                                        
         MVC   MYFULL,NBCMPSTR        SAVE COMPRESSED START/END DATE            
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK) GET TODAY'S DATE                      
         L     R2,=F'-61'                                                       
         GOTO1 ADDAY,DMCB,(0,WORK),(0,WORK),(R2)                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,NBCMPSTR)                                
         L     R2,=F'93'                                                        
         GOTO1 ADDAY,DMCB,(0,WORK),(0,WORK),(R2)                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,NBCMPEND)                                
         LA    R1,10               MONTHS                                       
         ST    R1,NMONTHS                                                       
         NETGO NVWKLST,DMCB,NMONTHS,DATEARE2,PERTYPE                            
*                                                                               
         MVC   NBCMPSTR(4),MYFULL  RESTORE SAVED COMPRESEED DATES               
         XIT1                                                                   
*                                                                               
                                                                                
*  HANDLE THE HEADER RECORD                                                     
DOHEAD   NTR1                                                                   
         MVI   FRST,1              SET FIRST REC                                
*                                                                               
         L     R1,RECTOT           UPDATE TOTAL REC COUNT                       
         LA    R1,2(R1)            FOR HEADER AND FOOTER                        
         ST    R1,RECTOT                                                        
*                                                                               
         MVC   OLDREC,NEWREC       SAVE NEW REC                                 
         BAS   RE,DOHEADER         CREATE HEADER REC                            
         XIT1                                                                   
*                                                                               
* CREATE HEADER LINE                                                            
DOHEADER NTR1                                                                   
         XC    OUTREC,OUTREC                                                    
         LA    R3,OUTREC                                                        
         USING PHEAD,R3                                                         
         MVC   PHHEAD,=CL10'HEADER'                                             
         MVC   PHHEAD2,=CL20'NETWORK ESTIMATES'                                 
         GOTO1 DATCON,DMCB,(5,0),(20,PHCREATE)  DATE REPORT REQUESTED           
                                                                                
* GET MOS START/END DATE OF DATE REPORT REQUESTED                               
         GOTO1 DATCON,DMCB,(5,0),(2,WORK)  TODAY -> 2 CHAR DATE                 
         LA    R2,DATEARE2  TABLE OF SPECIAL CALENDAR MOS                       
DOHEAD05 CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ???                                          
         CLC   WORK(2),2(R2)                                                    
         BH    DOHEAD10                                                         
         GOTO1 DATCON,DMCB,(2,0(R2)),(20,PHSTART)                               
         GOTO1 DATCON,DMCB,(2,2(R2)),(20,PHEND)                                 
         MVC   P(L'PHEAD),0(R3)                                                 
         BAS   RE,SENDIT                                                        
         B     DOHEADX                                                          
*                                                                               
DOHEAD10 LA    R2,4(R2)            BUMP MOS DATE TABLE                          
         B     DOHEAD05                                                         
*                                                                               
DOHEADX  XIT1                                                                   
         DROP  R3                                                               
*                                                                               
* CREATE FOOTER LINE                                                            
DOFOOTER NTR1                                                                   
         XC    OUTREC,OUTREC                                                    
         LA    R3,OUTREC                                                        
         USING PHFOOT,R3                                                        
         MVC   PHFOOT,=CL10'FOOTER'                                             
         EDIT  (B4,RECTOT),(10,PHFCOUNT)                                        
         LA    R1,PHFCOUNT                                                      
DOFT1    CLI   0(R1),X'40'                                                      
         BH    DOFT3                                                            
         MVI   0(R1),X'F0'                                                      
         LA    R1,1(R1)                                                         
         B     DOFT1                                                            
*                                                                               
DOFT3    EDIT  (P8,GRSTOTS),(16,PHFGRS),2                                       
         LA    R1,PHFGRS+1                                                      
         MVI   PHFGRS,C'+'                                                      
DOFT5    CLI   0(R1),X'40'                                                      
         BH    DOFT10                                                           
         MVI   0(R1),X'F0'                                                      
         LA    R1,1(R1)                                                         
         B     DOFT5                                                            
*                                                                               
DOFT10   EDIT  (P8,NETTOTS),(16,PHFNET),2                                       
         LA    R1,PHFNET+1                                                      
         MVI   PHFNET,C'+'                                                      
DOFT15   CLI   0(R1),X'40'                                                      
         BH    DOFT20                                                           
         MVI   0(R1),X'F0'                                                      
         LA    R1,1(R1)                                                         
         B     DOFT15                                                           
*                                                                               
DOFT20   DS    0H                                                               
         BAS   RE,SENDIT                                                        
DOFOOTX  XIT1                                                                   
         DROP  R3                                                               
*********************************************************************           
TESTRUN  DS    0H                                                               
         ICM   RF,15,TWAMASTC                                                   
         JNZ   *+6                                                              
         DCHO                                                                   
         ICM   RF,15,MCSSB-MASTD(RF)                                            
         JNZ   *+6                                                              
         DCHO                                                                   
         CLI   SSOXTND-SSOOFF(RF),X'FF'                                         
         JNE   NOTEST                                                           
         CLI   SSODSPAC-SSOOFF(RF),C'A'  IS THIS A TEST REQUEST?                
         JE    NOTEST                     NO                                    
         CLI   SSODSPAC-SSOOFF(RF),C' '  IS THIS A TEST REQUEST?                
         JNH   NOTEST                     NO                                    
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
NOTEST   CR    RE,RB                                                            
         BR    RE                                                               
*                                                                               
MQMSSG   NTR1                                                                   
*** MQOPEN  ********************************************************            
         ICM   RF,15,TWADCONS                                                   
         BNZ   *+6                                                              
         DCHO                                                                   
         ICM   RF,15,TMQRPT-TWADCOND(RF)                                        
         BNZ   *+6                                                              
         DCHO                                                                   
         ST    RF,AMQRPT                                                        
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
         BRAS  RE,TESTRUN          IS THIS A TEST RUN?                          
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
* NOTE THE MQ HEADER IS USED AS THE 'ROUTE'                                     
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),                               X        
               (0,=C'MEDIACOMSFTP****'),,0                                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*******************************************************************             
MQ       USING MQMSGD,ELEM                                                      
*                                                                               
         LA    R2,MQFILENM                                                      
         USING FILNAMD,R2                                                       
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         MVC   MQ.MQHID,=CL6'DANOT1'                                            
         MVC   MQ.MQSYS,=CL3'NET'                                               
*        MVC   MQ.MQAGYID,=CL4'MIND'                                            
         L     RF,TWAMASTC                                                      
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   MQ.MQAGYID,MCAGYCOD-MCEXTRA(RF)                                  
         MVC   MQ.MQQUAL,=CL32' '                                               
         MVC   MQ.MQQUAL(11),=C'SC ESTIMATE'                                    
****     MVC   MQ.MQQUAL,SPLTITL+16                                             
         OC    MQ.MQQUAL,=CL32' '                                               
         MVC   MQ.MQDATE,FNYYMMDD                                               
         MVC   MQ.MQTIME,FNTIME                                                 
*****    MVC   MQ.MQDATA1(L'WRIDESC),WRIDESC                                    
         MVC   MQ.MQDATA1,=CL32' '                                              
****     MVC   MQ.MQDATA1(26),SPLFLTH-41                                        
         OC    MQ.MQDATA1,=CL32' '                                              
         MVC   MQ.MQFILE(FILNAMX-FNPRG),FNPRG                                   
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*******************************************************************             
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DCHO                                                                   
         DROP  MQ                                                               
         DROP  R2                                                               
         LTORG                                                                  
*********************************************************************           
NETBUFF  DS    CL4000                                                           
*                                                                               
ESTABLE  DS    CL2800   ROOM FOR 200 (CL14) RECORDS                             
         DC    X'FF'                                                            
UCOMTBL  DS    CL9000   ROOM FOR 600 (CL15) RECORDS                             
         DC    X'FF'                                                            
       EJECT                                                                    
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                     ** = FROM EDIT MODULE                                     
RELO     DS    F      **                                                        
ANTWKTP  DS    F      **                                                        
RECTOT   DS    F                                                                
NMONTHS  DS    F                                                                
MYFULL   DS    F                                                                
AMQRPT   DS    F                                                                
GROSSNET DS    XL8                                                              
GRSTOTS  DS    PL8   GROSS DOLLARS TOTAL                                        
NETTOTS  DS    PL8   TEMP STORE AREA FOR NET $$$                                
FRST     DS    CL1                                                              
PERTYPE  DS    CL3                                                              
MQFILENM DS    CL44                                                             
*                                                                               
*                                                                               
NEWREC   DS    CL150                                                            
OLDREC   DS    CL150                                                            
OUTREC   DS    CL150                                                            
MYIO     DS    CL1500                                                           
DATEAREA DS    CL1000                                                           
DATEARE2 DS    CL50                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
PHEADER  DSECT                                                                  
PHEAD    DS    CL150                                                            
         ORG   PHEAD                                                            
PHHEAD   DS    CL10                CL10'HEADER'                                 
PHHEAD2  DS    CL20                CL20'NETWORK ESTIMAATES'                     
PHCREATE DS    CL8                 YYYYMMDD                                     
PHSTART  DS    CL8                 YYYYMMDD                                     
PHEND    DS    CL8                 YYMMDD                                       
PHHEADLN EQU   *-PHHEAD                                                         
         ORG   PHHEAD                                                           
PHDETAIL DS    CL10                CL10'DETAIL'                                 
PHURN    DS    CL8                 URN STORED ON UCOM                           
PHPROD   DS    CL3                 PRODUCT CODE                                 
PHEST    DS    CL8                 CL8'NNN'                                     
PHESTDAT DS    CL8                 YYYYNNDD                                     
PHESTDCR DS    CL30                PROD+MOS+SYSTEM/MEDIA                        
PHDETKLN EQU   *-PHDETAIL                                                       
PHNETASS DS    CL16                CL16'+0000000000100.00'                      
PHGRSASS DS    CL16                CL16'+0000000000100.00'                      
PHNETAS2 DS    CL16                CL16'+0000000000100.00'                      
PHDETLN  EQU   *-PHDETAIL                                                       
         ORG   PHHEAD                                                           
PHFOOT   DS    CL10                CL10'FOOTER'                                 
PHFCOUNT DS    CL10                TOTAL LINES DET+HEAD+FOOT                    
*                                  CL10'0000000007'                             
PHFGRS   DS    CL16                CL16'+0000000000100.00'                      
PHFNET   DS    CL16                CL16'+0000000000100.00'                      
PHFOOTLN EQU   *-PHFOOT                                                         
*                                                                               
FILNAMD  DSECT                                                                  
FNPFIX   DS    CL14                PREFIX (SFTPDISK.PROD.)                      
FNPRG    DS    CL3                 BUY                                          
         DS    CL1                 .                                            
FNSYS    DS    CL2                 NE                                           
         DS    CL1                 .                                            
FNAGYLBL DS    CL4                 AGY LABEL (CTAGCCOD)                         
         DS    CL2                 .D                                           
FNYYMMDD DS    CL6                 YYMMDD                                       
         DS    CL2                 .T                                           
FNTIME   DS    CL7                 HHMMSS                                       
FILNAMDQ EQU   *-FILNAMD                                                        
FILNAMX  EQU   *                                                                
*                                                                               
* NOTE: THE 'ROUTE' IS PASSED IN THE MQOPEN AS THE HEADER                       
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM=SPT                                   
MQAGYID  DS    CL4                 AGENCY ID                                    
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 DATE                                         
MQTIME   DS    CL6                 TIME                                         
MQDATA1  DS    CL32                                                             
MQDATA2  DS    CL32                                                             
MQFILE   DS    CL64                DSN                                          
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEBD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE DDUCOMD                                                        
       ++INCLUDE NEUCOMD                                                        
       ++INCLUDE NEDATELSTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088NEMED6B   10/26/12'                                      
         END                                                                    
