*          DATA SET SPSYN02    AT LEVEL 023 AS OF 05/01/02                      
*PHASE T21C02A,+0,NOAUTO                                                        
         TITLE 'T21C02 - SPOTPAK SYNDICATION - DISPLAY'                         
         PRINT NOGEN                                                            
T21C02   CSECT                                                                  
         NMOD1 0,T21C02                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21CFFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING SYNRECD,R8                                                       
* READ RECORD                                                                   
         MVI   ERRCD,NOFNDERR                                                   
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DISERR                                                           
*                                                                               
         MVI   ERRCD,DELERR                                                     
         TM    KEY+13,X'80'                                                     
         BO    DISERR                                                           
*                                                                               
         GOTO1 GETREC                                                           
         MVC   DUB(2),REC+13       GET LENGTH                                   
         LH    RE,DUB                                                           
         LA    RE,REC(RE)                                                       
         XC    0(2,RE),0(RE)       CLEAR LAST ELCODE/LEN                        
*                                                                               
         CLC   =C'BF',SYNLIN                                                    
         BE    DBF                                                              
* FIND LINE NUMBER IN RECORD                                                    
         LA    R4,SYNEL                                                         
         USING SYNCVEL,R4                                                       
         MVI   ELCODE,X'21'                                                     
         MVI   ERRCD,NOLNERR                                                    
         CLI   SVACT,C'M'                                                       
         BE    DSM                                                              
*                                                                               
DIS2     BAS   R9,NEXTEL                                                        
         BNE   DISERR                                                           
         CLC   SYNCVLIN,SVLIN                                                   
         BNE   DIS2                                                             
         ST    R4,CVELADDR         SAVE CVEL ADDR                               
* FIND NEXT CVEL ADDR                                                           
         BAS   R9,NEXTEL                                                        
         ST    R4,NEXTCVEL                                                      
*                                                                               
         L     R4,CVELADDR         RESTORE                                      
         EJECT                                                                  
* DISPLAY A LINE                                                                
*                                                                               
* DISPLAY MKT SHARE                                                             
         LH    R0,SYNCVPCT                                                      
         EDIT  (R0),(4,WORK2),2                                                 
         MVI   SYNSTNM+32,C'('                                                  
         MVC   SYNSTNM+33(4),WORK2                                              
         MVI   SYNSTNM+37,C')'                                                  
         FOUT  SYNSTNMH                                                         
*                                                                               
         LA    R2,SYNCPERH                                                      
         OC    SYNCVST,SYNCVST     TEST NO DATA                                 
         BNZ   DIS4                                                             
         MVC   8(17,R2),SPACES                                                  
         MVC   8(4,R2),=C'NONE'                                                 
         FOUT  (R2)                                                             
         B     DIS10                                                            
DIS4     DC    0H'0'                                                            
         MVC   WORK(6),SYNCVST                                                  
         BAS   R9,GETPER                                                        
         MVC   8(17,R2),WORK2                                                   
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNCDAYH                                                      
         MVC   8(8,R2),SYNCVDAY                                                 
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNCPTSH                                                      
         BAS   R9,GETPTS                                                        
         MVC   8(8,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNCDOLH                                                      
         BAS   R9,GETDOL                                                        
         MVC   8(8,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNCSEGH                                                      
         BAS   R9,GETSEG                                                        
         MVC   8(8,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
         CLI   1(R4),40            TEST GOAL DATA IN CVEL                       
         BL    DIS10                                                            
*                                                                               
         LA    R2,SYNGPTSH                                                      
         BAS   R9,GETGLPTS                                                      
         MVC   8(8,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNGDOLH                                                      
         BAS   R9,GETGLDOL                                                      
         MVC   8(8,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
* GUARANTEE DATA                                                                
DIS10    LA    R2,SYNGPERH                                                      
         BAS   R9,TSTDATA         CLEAR IF NEEDED                               
         LA    R2,SYNGAMTH                                                      
         BAS   R9,TSTDATA                                                       
*                                                                               
         L     R4,CVELADDR                                                      
         USING SYNGREL,R4                                                       
         MVI   ELCODE,X'31'                                                     
         BAS   R9,NEXTEL                                                        
         BNE   DIS20                                                            
         C     R4,NEXTCVEL                                                      
         BNL   DIS20                                                            
*                                                                               
         LA    R2,SYNGPERH                                                      
         MVC   WORK(6),SYNGRST                                                  
         BAS   R9,GETPER                                                        
         MVC   8(17,R2),WORK2                                                   
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNGAMTH                                                      
         BAS   R9,GETGRAMT                                                      
         MVC   8(8,R2),WORK2                                                    
         EJECT                                                                  
* COMMENTS                                                                      
*                                                                               
DIS20    LA    R2,SYNCOM1H                                                      
         BAS   R9,TSTDATA          CLEAR IF NEEDED                              
*                                                                               
         LA    R2,SYNCOM2H                                                      
         BAS   R9,TSTDATA                                                       
*                                                                               
         LA    R2,SYNCOM1H                                                      
         L     R4,CVELADDR                                                      
         USING SYNCOMEL,R4                                                      
         MVI   ELCODE,X'41'                                                     
DIS22    BAS   R9,NEXTEL                                                        
         BNE   DIS30                                                            
         C     R4,NEXTCVEL         TEST APPLIES TO THIS CVEL                    
         BNL   DIS30               NO                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R4)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SYNCOM                                                   
         FOUT  (R2)                                                             
*                                                                               
         SR    R0,R0               NEXT OUTPUT LINE                             
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DIS22               CHECK MORE COMMENTS                          
         EJECT                                                                  
DIS30    CLI   SVACT,C'A'          TEST ADD                                     
         BNE   DIS34                                                            
* DISPLAY ADDED LINE NUM                                                        
         LA    R2,SYNLINH                                                       
         SR    R0,R0                                                            
         IC    R0,SVLIN                                                         
         EDIT  (R0),(3,8(R2)),ALIGN=LEFT                                        
         FOUT  (R2)                                                             
*                                                                               
         MVC   SYNMSG(29),=C'DATA ADDED - NOTE LINE NUMBER'                     
         LA    R2,SYNPRH                                                        
         B     DIS38                                                            
*                                                                               
DIS34    CLI   SVACT,C'D'          TEST DISPLAY                                 
         BE    DSMX10                                                           
* ACTION MUST BE CHANGE                                                         
         LA    R2,SYNCPERH                                                      
*                                                                               
DIS36    MVC   SYNMSG(30),=C'DATA DISPLAYED - ENTER CHANGES'                    
         TM    SYNLINH+4,X'20'     TEST LINE PREVIOUSLY EDITED                  
         BZ    DIS38               NO - ASK FOR CHANGES                         
* LINE WAS EDITED BEFORE - THIS WAS A SUCCESSFUL EDIT                           
         LA    R2,SYNPRH                                                        
*                                                                               
         MVC   SYNMSG(30),=CL30'RECORD HAS BEEN UPDATED'                        
DIS38    OI    SYNLINH+4,X'20'     SET LINE EDITED                              
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY BAL FWD                                                               
*                                                                               
DBF      MVC   SYNSTNM+32(6),SPACES     CLEAR MKT SHARE                         
         FOUT  SYNSTNMH                                                         
*                                                                               
         LA    R4,SYNEL                                                         
         USING SYNBFEL,R4                                                       
         MVI   ELCODE,X'11'                                                     
         MVI   ERRCD,NOLNERR                                                    
         BAS   R9,NEXTEL                                                        
         BNE   DISERR                                                           
         OI    SYNLINH+4,X'20'     SET EDITED FLAG                              
*                                                                               
         LA    R2,SYNBPERH                                                      
         GOTO1 VDATCON,DMCB,(3,SYNBFDT),(5,8(R2))                               
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNBPTSH                                                      
         L     R0,SYNBFPTS                                                      
         BAS   R9,GETPTS2                                                       
         MVC   8(8,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNBDOLH                                                      
         L     R0,SYNBFDOL                                                      
         BAS   R9,GETDOL2                                                       
         MVC   8(8,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   DBF12                                                            
*                                                                               
         MVC   SYNMSG(21),=C'BALANCE FORWARD ADDED'                             
         LA    R2,SYNPRH                                                        
         B     EXIT                                                             
*                                                                               
DBF12    CLI   SVACT,C'D'                                                       
         BE    DSMX10                                                           
*                                                                               
         LA    R2,SYNBPERH                                                      
         B     DIS36                                                            
         EJECT                                                                  
* MULTIPLE LINE DISPLAY                                                         
*                                                                               
DSM      MVC   SYNSTNM+32(6),SPACES   CLEAR MKT SHARE ON SCREEN                 
         FOUT  SYNSTNMH                                                         
* FIND FIRST AND NEXT CVEL                                                      
         USING SYNCVEL,R4                                                       
         BAS   R9,NEXTEL                                                        
         BNE   DISERR                                                           
         CLC   SYNCVLIN,SVLIN                                                   
         BL    *-14                                                             
         ST    R4,CVELADDR                                                      
         BAS   R9,NEXTEL           FIND NEXT CVEL                               
         ST    R4,NEXTCVEL                                                      
*                                                                               
         LA    R2,SYNUPDTH                                                      
* DISPLAY ACTIVITY                                                              
         LA    R4,SYNEL                                                         
         USING SYNACTEL,R4                                                      
         MVC   8(7,R2),=C'CREATED'                                              
         GOTO1 VDATCON,DMCB,(3,SYNACTCR),(5,16(R2))                             
*                                                                               
         MVC   25(7,R2),=C'UPDATED'                                             
         GOTO1 (RF),(R1),(3,SYNACTDT),(5,33(R2))                                
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,SYNLN1H          FIRST DISPLAY LINE                           
         USING DSMH,R2                                                          
* BAL FWD (IF ANY)                                                              
         LA    R4,SYNEL                                                         
         USING SYNBFEL,R4                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   R9,NEXTEL                                                        
         BNE   DSM10                                                            
         MVC   8(68,R2),SPACES                                                  
         MVC   DSMTYPE,=C'BF'                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(3,SYNBFDT),(5,DSMPER)                              
*                                                                               
         L     R0,SYNBFPTS                                                      
         BAS   R9,GETPTS2                                                       
         MVC   DSMPTS,WORK2                                                     
*                                                                               
         L     R0,SYNBFDOL                                                      
         BAS   R9,GETDOL2                                                       
         MVC   DSMDOL,WORK2                                                     
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         EJECT                                                                  
* COVERAGE DATA                                                                 
DSM10    L     R4,CVELADDR                                                      
         USING SYNCVEL,R4                                                       
*                                                                               
         MVC   8(68,R2),SPACES                                                  
*                                                                               
         MVC   DSMTYPE,=C'CV'                                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SYNCVLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSMLINE,DUB                                                      
*                                                                               
         OC    SYNCVST,SYNCVST                                                  
         BNZ   *+14                                                             
         MVC   DSMPER(8),=C'* NONE *'                                           
         B     DSM20                                                            
         MVC   WORK(6),SYNCVST                                                  
         BAS   R9,GETPER                                                        
         MVC   DSMPER,WORK2                                                     
*                                                                               
         LH    R0,SYNCVPCT         MARKET SHARE                                 
         EDIT  (R0),(4,DSMSHR),2                                                
*                                                                               
         MVC   DSMDAY,SYNCVDAY                                                  
*                                                                               
         BAS   R9,GETPTS                                                        
         MVC   DSMPTS,WORK2                                                     
*                                                                               
         BAS   R9,GETDOL                                                        
         MVC   DSMDOL,WORK2                                                     
*                                                                               
         BAS   R9,GETSEG                                                        
         MVC   DSMSEG,WORK2                                                     
*                                                                               
DSM20    FOUT  (R2)                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXIT                                                             
         EJECT                                                                  
* DISPLAY GOAL DATA (IF ANY)                                                    
         CLI   1(R4),40            TEST GOAL DATA IN CVEL                       
         BL    DSM25               NO                                           
         OC    SYNGLDOL(8),SYNGLDOL                                             
         BZ    DSM25                                                            
*                                                                               
         MVC   8(68,R2),SPACES                                                  
         MVC   DSMTYPE,=C'GL'                                                   
         BAS   R9,GETGLPTS                                                      
         MVC   DSMPTS,WORK2                                                     
*                                                                               
         BAS   R9,GETGLDOL                                                      
         MVC   DSMDOL,WORK2                                                     
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXIT                                                             
         EJECT                                                                  
* GUARANTEE DATA (IF ANY)                                                       
*                                                                               
DSM25    L     R4,CVELADDR                                                      
         USING SYNGREL,R4                                                       
         MVI   ELCODE,X'31'                                                     
         BAS   R9,NEXTEL                                                        
         C     R4,NEXTCVEL                                                      
         BNL   DSM30                                                            
*                                                                               
         MVC   DSMTYPE,=C'GR'                                                   
*                                                                               
         MVC   WORK(6),SYNGRST                                                  
         BAS   R9,GETPER                                                        
         MVC   DSMPER,WORK2                                                     
*                                                                               
         BAS   R9,GETGRAMT                                                      
         MVC   DSMDOL,WORK2                                                     
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXIT                                                             
         SPACE 2                                                                
* UPDATE CVELADDR/NEXTCVEL                                                      
*                                                                               
DSM30    MVC   CVELADDR,NEXTCVEL                                                
         L     R4,CVELADDR                                                      
         CLI   0(R4),X'21'         DIS WE PROCESS LAST CVEL                     
         BNE   DSMX                YES - DONE                                   
         MVI   ELCODE,X'21'                                                     
         BAS   R9,NEXTEL                                                        
         ST    R4,NEXTCVEL         SAVE ADDR OF NEXT CVEL                       
         B     DSM10                                                            
         SPACE 2                                                                
DSMX     BAS   R9,TSTDATA          CLEAR REMAINING SCREEN LINES                 
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   DSMX                                                             
*                                                                               
DSMX10   LA    R2,SYNPRH                                                        
         MVC   SYNMSG(24),=C'REQUESTED DATA DISPLAYED'                          
         OI    SYNLINH+4,X'20'     SET LINE EDITED                              
         B     EXIT                                                             
         EJECT                                                                  
GETPER   GOTO1 VDATCON,DMCB,(3,WORK),(5,WORK2)                                  
*                                                                               
         MVI   WORK2+8,C'-'                                                     
*                                                                               
         GOTO1 (RF),(R1),(3,WORK+3),(5,WORK2+9)                                 
*                                                                               
         BR    R9                                                               
         SPACE 2                                                                
         USING SYNCVEL,R4                                                       
GETPTS   DS    0H                                                               
         L     R0,SYNCVPTS                                                      
         B     *+8                                                              
GETGLPTS L     R0,SYNGLPTS                                                      
         MVC   WORK2,SPACES                                                     
         CLI   SVGOAL+1,X'FF'        TEST DOLLARS ONLY                          
         BER   R9                                                               
GETPTS2  EDIT  (R0),(8,WORK2),ALIGN=LEFT,FLOAT=-                                
*                                                                               
         CLI   WORK2,C' '          EQUIVALENT TO ZERO=NOBLANK                   
         BNE   *+8                 AND IT ALWAYS WORKS                          
         MVI   WORK2,C'0'                                                       
         BR    R9                                                               
         DROP  R4                                                               
         SPACE 2                                                                
         USING SYNCVEL,R4                                                       
GETDOL   L     R0,SYNCVDOL                                                      
         B     *+8                                                              
GETGLDOL L     R0,SYNGLDOL                                                      
*                                                                               
GETDOL2  SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(8,WORK2),ALIGN=LEFT,FLOAT=-                                
*                                                                               
         CLI   WORK2,C' '          EQUIVALENT TO ZERO=NOBLANK                   
         BNE   *+8                 AND IT ALWAYS WORKS                          
         MVI   WORK2,C'0'                                                       
         BR    R9                                                               
         DROP  R4                                                               
         EJECT                                                                  
         USING SYNCVEL,R4                                                       
GETSEG   MVC   WORK2,SPACES                                                     
         CLC   =H'100',SYNCVSEG                                                 
         BER   R9                                                               
*                                                                               
         CLC   =H'10000',SYNCVSEG                                               
         BER   R9                                                               
*                                                                               
         LH    R0,SYNCVSEG                                                      
         EDIT  (R0),(8,WORK2),2,ALIGN=LEFT                                      
*                                                                               
         BR    R9                                                               
         DROP  R4                                                               
         SPACE 2                                                                
         USING SYNGREL,R4                                                       
GETGRAMT L     R0,SYNGRAMT                                                      
         CLI   SYNGRIPT,C'$'                                                    
         BE    GETDOL2                                                          
* AMOUNT IS PCT                                                                 
         EDIT  (R0),(8,WORK2),ALIGN=LEFT                                        
         LA    RE,WORK2                                                         
         AR    RE,R0                                                            
         MVC   0(3,RE),=C'PCT'                                                  
         BR    R9                                                               
         EJECT                                                                  
DISERR   GOTO1 ERROR                                                            
*                                                                               
EXIT     OI    6(R2),X'40'         INSERT CURSOR                                
         XMOD1 1                                                                
         SPACE 2                                                                
* CLEAR AND FOUT DATA LINE IF NECESSARY                                         
*                                                                               
TSTDATA  SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BZ    TSTDATAX                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BE    TSTDATAX                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         FOUT  (R2)                                                             
*                                                                               
TSTDATAX BR    R9                                                               
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R4),ELCODE                                                   
         BER   R9                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   R9,R9               SET CC NOT EQUAL                             
         BR    R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SPSYNWRK                                                                      
       ++INCLUDE SPSYNWRK                                                       
         EJECT                                                                  
RECD     DSECT                                                                  
* SPGENSYN                                                                      
       ++INCLUDE SPGENSYN                                                       
         EJECT                                                                  
* DSECT FOR DSM LINE                                                            
*                                                                               
DSMH     DSECT                                                                  
         DS    CL8                 HEADER                                       
DSMTYPE  DS    CL2                 LINE TYPE (CV/BF)                            
         DS    CL2                                                              
DSMLINE  DS    CL2                 LINE NUM                                     
         DS    CL2                                                              
DSMPER   DS    CL17                PERIOD                                       
         DS    CL2                                                              
DSMSHR   DS    CL4                                                              
         DS    CL2                                                              
DSMDAY   DS    CL8                 DAY/TIME                                     
         DS    CL2                                                              
DSMPTS   DS    CL8                 POINTS                                       
         DS    CL2                                                              
DSMDOL   DS    CL8                 DOLLARS                                      
         DS    CL2                                                              
DSMSEG   DS    CL5                 SEG PCT                                      
 END                                                                            
