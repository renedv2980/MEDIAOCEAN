*          DATA SET SRRUN00    AT LEVEL 016 AS OF 05/12/20                      
*PHASE T15800A                                                                  
         TITLE 'SRRUN00 - DISPLAY/CHANGE SCHEDULER JOB QUEUE'                   
*&&      SET   NOP=N                                                            
RUN      CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**$RUN**,RA,CLEAR=YES,RR=RE                                
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         L     R9,ACOMMON          SET A(COMMON STORAGE)                        
         A     R9,RELO                                                          
         USING COMMON,R9                                                        
*                                                                               
         MVC   SRPARMS,0(R1)                                                    
         L     R8,SRPATWA                                                       
         USING RUNTWAD,R8          R8=A(TWA)                                    
         L     R7,SRPATIA                                                       
         USING SRSD,R7             R7=A(SPECIAL S/R SAVE PAGE)                  
JT       USING DMSPACED,DSPHD                                                   
CL       USING DMSPACED,CSPHD                                                   
ACT      USING ACTTABD,ACTNTRY                                                  
*                                                                               
         BRAS  RE,INIT             INITIALISE                                   
*                                                                               
         BRAS  RE,RDTWAB           READ TWA 11                                  
*                                                                               
         CLI   PFKEY,9             GOTO =PQ                                     
         BE    GOPQ                                                             
*                                                                               
RUN001   BRAS  RE,VALP1            VALIDATE ACTION                              
         BNE   XMOD                                                             
*                                                                               
         CLI   PFKEY,4             PF4 CYCLES SCREENS                           
         BNE   RUN005                                                           
*                                                                               
         L     R3,AACTTAB          FIND ACTION IN TABLE                         
         USING ACTTABD,R3                                                       
RUN002   CLC   ACT.ACTACT,ACTACT                                                
         BE    RUN003                                                           
         AHI   R3,ACTTABL                                                       
         CLI   0(R3),X'FF'                                                      
         BE    RUN005                                                           
         B     RUN002                                                           
*                                                                               
RUN003   AHI   R3,ACTTABL                                                       
         CLI   0(R3),X'FF'         ROLL ROUND IF END                            
         BNE   *+8                                                              
         L     R3,AACTTAB          FIND ACTION IN TABLE                         
*                                                                               
         CLI   ACTACT,ACTTRST      SKIP ACTION RESET                            
         BE    RUN003                                                           
*                                                                               
         TM    ACTFLAG,ACTFDIS     FIND DISPLAY TYPE ACTION                     
         BNO   RUN003                                                           
*                                                                               
         MVC   ACTNTRY,ACTTABD                                                  
         LH    R1,ACTUNAM                                                       
         AR    R1,RC                                                            
         MVC   RUNACT(8),0(R1)                                                  
*                                                                               
RUN005   BRAS  RE,VALP2            VALIDATE OPTIONS                             
*                                                                               
RUN010   MVI   FNDX,0                                                           
         BRAS  RE,MAIN                                                          
         B     XMOD                                                             
*                                                                               
ACOMMON  DC    A(COMMON)                                                        
         EJECT                                                                  
***********************************************************************         
* PROCESS INPUT ACTION                                                *         
***********************************************************************         
MAIN     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
MAIN005  LA    R6,SR$RUN                                                        
         USING RUNSAVED,R6         R6=A($RUN SAVE AREA)                         
         MVC   RUNSACT,ACT.ACTACT                                               
*                                                                               
         CLI   ACT.ACTACT,ACTTRST  TEST RESET                                   
         BNE   MAIN007             NO                                           
*                                                                               
         BRAS  RE,RESET            RESET QUEUE                                  
*                                                                               
         MVC   RUNACT,=CL8'Display'                                             
         MVI   RUNACTH+5,X'08'     input length                                 
         OI    RUNACTH+6,X'80'     transmit                                     
         L     RF,=A(ACTDIS)       point to action 'Display'                    
         A     RF,RELO                                                          
         MVC   ACTNTRY,0(RF)                                                    
         MVC   RUNSACT,ACT.ACTACT                                               
         B     MAIN009             perform action Display                       
*                                                                               
MAIN007  DS    0H                                                               
         CLI   ACT.ACTACT,ACTTSUM  TEST SUMMARY                                 
         BNE   *+12                NO                                           
         BRAS  RE,SUMMARY          DISPLAY SUMMARY                              
         B     DONE                                                             
*                                                                               
         CLI   ACT.ACTACT,ACTTSUK  TEST SUMMARY                                 
         BNE   *+12                NO                                           
         BRAS  RE,SUKMARY          DISPLAY UK SUMMARY                           
         B     DONE                                                             
*                                                                               
         CLI   ACT.ACTACT,ACTTPRI  TEST PRIORITY                                
         BNE   *+12                NO                                           
         BRAS  RE,PRIORITY         SET NEW PRIORITIES                           
         B     IPRI                                                             
*                                                                               
         CLI   ACT.ACTACT,ACTTPUR  TEST PURGE                                   
         BNE   *+12                NO                                           
         BRAS  RE,PRGE             SET PURGE JOB                                
         B     IPUR                                                             
*                                                                               
         CLI   ACT.ACTACT,ACTTSTA  TEST STATS                                   
         BNE   *+18                                                             
         BRAS  RE,STATS            YEP                                          
         MVC   SEQDIS,=H'1'                                                     
         B     MAIN010                                                          
*                                                                               
         CLI   ACT.ACTACT,ACTTQUE  TEST QUEUE                                   
         BE    SUKMARY             YEP                                          
*                                                                               
         CLI   ACT.ACTACT,ACTTLIS  TEST LIST                                    
         BNE   MAIN009                                                          
         OC    JCLFILT,JCLFILT                                                  
         BNZ   *+12                                                             
         BRAS  RE,LISTREP          YEP                                          
         B     MAIN010                                                          
*                                                                               
         BRAS  RE,LISTJCL          JCL FILTER SO READ BUFFER                    
         B     MAIN010                                                          
*                                                                               
MAIN009  BRAS  RE,DIS              MUST BE DISPLAY                              
         BNE   ENTD                                                             
*                                                                               
*                                                                               
MAIN010  LA    R1,RUNACTH          SET FADR FOR ERROR/EXIT                      
         ST    R1,FADR                                                          
         OC    SEQDIS,SEQDIS       TEST ANYTHING DISPLAYED                      
         BZ    ENTD                NO                                           
*                                                                               
         XC    WORK(20),WORK       FORMAT MESSAGE IN FLD FOR EXIT               
         MVI   WORK+0,4                                                         
         MVI   WORK+4,4                                                         
         MVI   WORK+8,5                                                         
         LH    R0,SEQLO                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(3),DUB                                                    
*                                                                               
         LH    R0,SEQHI                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+5(3),DUB                                                    
*                                                                               
         L     R0,TABCOUNT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+9(4),DUB                                                    
*                                                                               
         MVC   FLD+L'DISMSG(L'BIGMSG),BIGMSG                                    
         L     R0,TABCOUNT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FLD+55(4),DUB                                                    
*                                                                               
GO3      CLI   RUNOPTH+5,0                                                      
         B     GOX                                                              
*                                                                               
GOX      DS    0H                                                               
         MVC   RUNSLO,SEQLO        PUT IN S/R SAVED STORAGE                     
         MVC   RUNSHI,SEQHI                                                     
         MVC   RUNSNUM,SEQNUM                                                   
         B     IJOB                GO SET MESSAGE & CURSOR                      
         EJECT                                                                  
***********************************************************************         
* DISPLAY JOB QUEUE                                                   *         
***********************************************************************         
DIS      NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
         SAM31                                                                  
         MVCDD RUNHED,SR#RUN02     DDS HEADERS                                  
         MVCDD RUNHEDU,SR#RUN03                                                 
*                                                                               
         CLI   DDS,YES                                                          
         BNE   *+12                                                             
         BRAS  RE,DFHDR            DISPLAY DDS HEADER                           
         B     *+8                                                              
         BRAS  RE,UFHDR            DISPLAY USER HEADER                          
*                                                                               
         XC    SEQS,SEQS           CLEAR DISPLAY SEQUENCE NUMBERS               
         XC    TABCOUNT,TABCOUNT   CLEAR TABLE ENTRY COUNTER                    
         BRAS  RE,BLDJOBT          COPY JOB TAB TO TSAR BUFFER                  
*                                                                               
         BRAS  RE,SCROLL           VALIDATE SCROLL PFKEYS IF SET                
*                                                                               
         BRAS  RE,ARSOFF                                                        
         SAM31                                                                  
         ICM   R2,15,ABIGBUF       A(LOCAL SORTED JOB TABLE)                    
         USING TBJOBTAB,R2                                                      
         LA    R3,RUNLINH                                                       
         USING RUNLINED,R3                                                      
         USING FHD,RUNLLINH                                                     
*                                                                               
DIS16    CLI   FHLN,0              END OF SCREEN?                               
         BE    DISX                YES                                          
         OC    TBJCLASS,TBJCLASS   END OF TABLE?                                
         BZ    DISX                YES                                          
*                                                                               
         LH    RF,SEQNUM           BUMP SEQUENCE NUMBER                         
         AHI   RF,1                                                             
         STH   RF,SEQNUM                                                        
*                                                                               
         CH    RF,STARTPF          WHERE TO START FOR PF SCROLLING              
         BNL   *+12                                                             
         AH    R2,JOBTABL          NEXT ENTRY                                   
         B     DIS16                                                            
*                                                                               
         LH    R1,SEQDIS                                                        
         AHI   R1,1                                                             
         STH   R1,SEQDIS                                                        
*                                                                               
         CLI   DDS,YES             DDS?                                         
         BNE   DIS18               NO                                           
*                                                                               
         CLI   PFKEY,2             PF2 = SHOW CONTENTS OF PQ                    
         BNE   DIS17                                                            
         CLC   FHAD,CURSORD                                                     
         BNE   *+14                                                             
         LR    R1,R2                                                            
         BRAS  RE,SELJOB                                                        
         B     EXITOK                                                           
*                                                                               
DIS17    CLI   DDS,YES             DDS OR CLIENT                                
         BNE   DIS18                                                            
         BRAS  RE,DFORMAT          FORMAT DDS DISPLAY LINE                      
         B     DIS20                                                            
*                                                                               
DIS18    BRAS  RE,UFORMAT          FORMAT USER DISPLAY LINE                     
*                                                                               
DIS20    OC    SEQLO,SEQLO         SET DISPLAY SEQUENCE NUMBERS                 
         BNZ   *+10                                                             
         MVC   SEQLO,SEQNUM                                                     
         MVC   SEQHI,SEQNUM                                                     
*                                                                               
         XR    RF,RF               GO TO NEXT LINE                              
         IC    RF,FHLN                                                          
         AR    R3,RF                                                            
         LA    R0,RUNXXXH                                                       
         CR    R3,R0                                                            
         BNL   DISX                                                             
*                                                                               
         AH    R2,JOBTABL          GO TO NEXT ENTRY                             
         B     DIS16                                                            
*                                                                               
DISX     CLI   PFKEY,2                                                          
         BNE   EXITOK                                                           
         ICM   R1,15,=XL4'00032C00'                                             
         BRAS  RE,SELJOB                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT A DISPLAY LINE                                    *         
*                                                                     *         
* NTRY - R4=A(JOB TABLE ENTRY)                                        *         
* NTRY - R2=A(MY JOB TABLE ENTRY)                                     *         
*                                                                     *         
* EXIT - CC=NOT EQUAL IF JOB WON'T FIT ON SCREEN                      *         
***********************************************************************         
DFORMAT  NTR1  BASE=*,LABEL=*                                                   
         USING TBJOBTAB,R2                                                      
         USING RUNLINED,R3                                                      
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,TBJSTIME                                                    
         CLI   TBJSTIME,X'FF'      DISPLAY TIME REBUILT BY RERUN?               
         BNE   DFM02                                                            
*                                                                               
         N     RF,=X'0000FFFF'                                                  
         XR    RE,RE                                                            
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
*                                                                               
DFM02    A     RF,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         BRAS  RE,SHOWTIME                                                      
         MVC   RUNLTIME(5),WORK2                                                
*                                                                               
         MVC   RUNLCLS,TBJCLASS    SOON CLASS (E.G., A3SOON)                    
         MVC   RUNLAGY,TBJAGY      SOON CLASS (E.G., A3SOON)                    
         MVC   RUNLNAME,TBJMVSID   EDIT MVS JOB ID (UUUUSSPP)                   
         MVC   RUNLPRTY,TBJPRTY    JOB PRIORITY                                 
*                                                                               
         MVC   RUNLTYP,=CL2'  '                                                 
         MVC   RUNLTYP(1),TBJCTYPE                                              
         TM    TBJSTAT,JOBFUPDT                                                 
         BZ    *+8                                                              
         MVI   RUNLTYP+1,C'U'      INDICATE UPDATIVE                            
*                                                                               
         MVC   BYTE,TBJADV                                                      
         NI    BYTE,X'0F'                                                       
         XR    RF,RF                                                            
         IC    RF,BYTE                                                          
         MHI   RF,L'FACITAB                                                     
         A     RF,AFID                                                          
         USING FACITABD,RF                                                      
         MVC   RUNLADV,FACISN4                                                  
         DROP  RF                                                               
*                                                                               
         MVC   RUNLMONS,TBJMONS                                                 
         MVC   RUNLTSYM,TBJLUID                                                 
         MVC   RUNLPQNO,TBJPQID    PRINT QUEUE                                  
         TM    TBJSTAT,JOBFUPDT                                                 
         BZ    *+8                                                              
         MVI   RUNLPQNO+1,C'U'                                                  
         TM    TBJSTAT,JOBFLONG                                                 
         BZ    *+8                                                              
         MVI   RUNLPRTY+1,C'L'                                                  
*                                                                               
         MVC   HALF,TBJPQUSR       USER NAME                                    
         BRAS  RE,GETUNAM                                                       
         MVC   RUNLUSER,WORK2                                                   
*                                                                               
         MVC   RUNLREPT,TBJPQSUB                                                
         MVI   RUNLRSD,RUNLRSDQ                                                 
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,TBJPQSEQ                                                    
         EDIT  (R0),(5,RUNLSEQN),ALIGN=LEFT                                     
*                                                                               
         L     R1,ASTATAB                                                       
         LA    RF,=CL3'???'                                                     
DFM08    CLI   0(R1),0             TEST E-O-T                                   
         BE    DFM10                                                            
         MVC   BYTE,TBJSTAT                                                     
         NI    BYTE,255-TBJIGNOR                                                
         NC    BYTE,4(R1)                                                       
         BNZ   *+12                                                             
         AHI   R1,L'STATAB                                                      
         B     DFM08                                                            
*                                                                               
         EX    0,0(R1)                                                          
*                                                                               
DFM10    MVC   RUNLSTAT,0(RF)                                                   
         NC    RUNLSTAT+1(2),=X'BFBF'                                           
*                                                                               
         TM    TBJSTAT,JOBFRUN     ONLY IF RUNNING                              
         BZ    EXITOK                                                           
*                                                                               
         XR    R0,R0               DISPLAY TIME RUNNING                         
         ICM   R0,7,TBJRTIME                                                    
         BZ    EXITOK                                                           
         L     RF,CURTIME                                                       
         SR    RF,R0                                                            
         BNP   EXITOK                                                           
         BRAS  RE,SHOWWAIT                                                      
         MVC   RUNLSTAT(5),WORK2                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT A USER DISPLAY LINE                               *         
* NTRY - R4=A(JOB TABLE ENTRY)                                        *         
* NTRY - R2=A(MY JOB TABLE ENTRY)                                     *         
*                                                                     *         
* EXIT - CC=NOT EQUAL IF JOB WON'T FIT ON SCREEN                      *         
***********************************************************************         
UFORMAT  NTR1  BASE=*,LABEL=*                                                   
         USING TBJOBTAB,R2                                                      
         USING USRLINED,R3         R3=A(TWA DISPLAY LINE)                       
*        MVCDD RUNHED,SR#RUN04                                                  
*        MVCDD RUNHEDU,SR#RUN05                                                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,TBJSTIME                                                    
         CLI   TBJSTIME,X'FF'      DISPLAY TIME REBUILT BY RERUN?               
         BNE   UFM02                                                            
         N     RF,=X'0000FFFF'                                                  
         XR    RE,RE                                                            
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
*                                                                               
UFM02    A     RF,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         BRAS  RE,SHOWTIME                                                      
         MVC   USRLTIME,WORK2                                                   
*                                                                               
         MVC   HALF,TBJPQUSR       USER NAME                                    
         BRAS  RE,GETUNAM                                                       
         MVC   USRLUSER,WORK2                                                   
*                                                                               
         MVC   USRLREPT,TBJPQSUB                                                
         MVI   USRLRSD,USRLRSDQ                                                 
         XR    R0,R0                                                            
         ICM   R0,3,TBJPQSEQ                                                    
         EDIT  (R0),(5,USRLSEQN),ALIGN=LEFT                                     
*                                                                               
         TM    TBJSTAT,JOBFUPDT+JOBFLONG                                        
         BNO   *+14                                                             
         MVC   USRLTYPE,=CL10'Long,Upd'                                         
         B     UFM04                                                            
*                                                                               
         TM    TBJSTAT,JOBFLONG                                                 
         BNO   *+14                                                             
         MVC   USRLTYPE,=CL10'Long'                                             
         B     UFM04                                                            
*                                                                               
         TM    TBJSTAT,JOBFUPDT                                                 
         BNO   *+14                                                             
         MVC   USRLTYPE,=CL10'Normal,Upd'                                       
         B     UFM04                                                            
*                                                                               
         MVC   USRLTYPE,=CL10'Normal'                                           
*                                                                               
UFM04    L     R1,ASTATAB                                                       
         LA    RF,=CL8'????????'                                                
UFM08    CLI   0(R1),0             TEST E-O-T                                   
         BE    UFM10                                                            
         MVC   BYTE,TBJSTAT                                                     
         NC    BYTE,4(R1)                                                       
         BNZ   *+12                                                             
         LA    R1,L'STATAB(R1)                                                  
         B     UFM08                                                            
*                                                                               
         EX    0,0(R1)                                                          
*                                                                               
UFM10    MVC   USRLSTAT,0(RF)                                                   
         NC    USRLSTAT+1(8),=X'BFBFBFBFBFBFBFBF'                               
*                                                                               
UFM12    TM    TBJSTAT,JOBFRUN     ONLY IF RUNNING                              
         BZ    EXITOK                                                           
*                                                                               
         XR    R0,R0               DISPLAY TIME RUNNING                         
         ICM   R0,7,TBJRTIME                                                    
         BZ    EXITOK                                                           
         L     RF,CURTIME                                                       
         SR    RF,R0                                                            
         BNP   EXITOK                                                           
         BRAS  RE,SHOWWAIT                                                      
         MVC   USRLSTAT+10(5),WORK2                                             
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* COPY JOBTAB TO XA WORKING STORAGE (IN TSAR BUFFER)                  *         
***********************************************************************         
BLDJOBT  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
         SAM31                                                                  
         L     RF,MYSSB                                                         
         ICM   R1,15,SSBTSAR-SSBD(RF)                                           
         L     R0,ABIGBUF                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTEND                                                 
         ST    R2,AJOBTABX                                                      
         ICM   R2,15,JT.DSPTFRST                                                
         ST    R2,AJOBTAB                                                       
         AH    R2,JOBHDRL                                                       
         SAC   512                                                              
         USING TBJOBTAB,R2         R2 = TABS DATASPACE JOB TABLE                
*                                                                               
         XR    R3,R3               R3 = COUNT OF COPIED JOBS                    
         L     R4,ABIGBUF                                                       
B        USING TBJOBTAB,R4         R4 = TSAR JOB TABLE COPY                     
*                                                                               
BLJ02    OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BZ    BLJ22                                                            
         CLC   TBJCLASS,EFFS                                                    
         BE    BLJ22                                                            
         CLI   TBJSTAT,0           IF FLAGGED FOR DELETE                        
         BE    BLJ22                                                            
*                                                                               
         OC    TBJTERM,TBJTERM     THERE MUST BE A TERMINAL NUMBER. . .         
         BZ    BLJ22                                                            
         OC    TBJPQKEY,TBJPQKEY   . . . AND REPORT ID. . .                     
         BZ    BLJ22                                                            
*                                                                               
         CLI   JOBFILT,0                                                        
         BNE   BLJ04                                                            
         CLI   ACT.ACTACT,ACTTLIS  UNLESS ACTION LIST                           
         BE    BLJ04                                                            
         OC    TBJETIME,TBJETIME   DEFAULT TO SUB AND RUN                       
         BNZ   BLJ22                                                            
         B     BLJ10                                                            
*                                                                               
BLJ04    CLI   JOBFILT,JOBFAVA                                                  
         BNE   BLJ06                                                            
         OC    TBJETIME,TBJETIME                                                
         BZ    BLJ22                                                            
         B     BLJ10                                                            
*                                                                               
BLJ06    CLI   JOBFILT,JOBFRUN                                                  
         BNE   BLJ08                                                            
         OC    TBJETIME,TBJETIME                                                
         BNZ   BLJ22                                                            
         OC    TBJRTIME,TBJRTIME                                                
         BZ    BLJ22                                                            
         B     BLJ10                                                            
*                                                                               
BLJ08    CLI   JOBFILT,JOBFSUB                                                  
         BNE   BLJ09                                                            
         OC    TBJETIME,TBJETIME                                                
         BNZ   BLJ22                                                            
         OC    TBJRTIME,TBJRTIME                                                
         BNZ   BLJ22                                                            
*                                                                               
BLJ09    CLI   JOBFILT,JOBFHOLD                                                 
         BNE   BLJ09A                                                           
         TM    TBJSTAT,TBJHOLD                                                  
         BNO   BLJ22                                                            
*                                                                               
BLJ09A   CLI   JOBFILT,JOBFKILL                                                 
         BNE   BLJ10                                                            
         TM    TBJSTAT,TBJKILL                                                  
         BNO   BLJ22                                                            
*                                                                               
BLJ10    OC    TERMLUID,TERMLUID   TERMINAL FILTER                              
         BZ    *+14                                                             
         CLC   TBJLUID,TERMLUID                                                 
         BNE   BLJ22                                                            
*                                                                               
         CLI   JOBSYS,0            SYSTEM FILTER                                
         BE    *+14                                                             
         CLC   JOBSYS,TBJMVSID+4                                                
         BNE   BLJ22                                                            
*                                                                               
         OC    JOBTYPE,JOBTYPE     JOBTYPE FILTER (SYS/SUB ID)                  
         BZ    *+14                                                             
         CLC   JOBTYPE,TBJMVSID+4                                               
         BNE   BLJ22                                                            
*                                                                               
         OC    JOBCLS,JOBCLS       CLASS FILTER                                 
         BZ    BLJ12                                                            
         LHI   RF,2-1              LENGTH OF CLASS CODE                         
         EX    RF,*+8                                                           
         BNE   BLJ22                                                            
         CLC   JOBCLS(0),TBJCLASS                                               
*                                                                               
BLJ12    OC    JOBUSER,JOBUSER     USERID FILTER                                
         BZ    *+14                                                             
         CLC   JOBUSER,TBJPQUSR                                                 
         BNE   BLJ22                                                            
*                                                                               
         OC    JOBAGY,JOBAGY       AGY FILTER                                   
         BZ    *+14                                                             
         CLC   JOBAGY,TBJAGY                                                    
         BNE   BLJ22                                                            
*                                                                               
         OC    JOBSUBID,JOBSUBID   SUB-ID FILTER                                
         BZ    *+14                                                             
         CLC   JOBSUBID,TBJPQSUB                                                
         BNE   BLJ22                                                            
*                                                                               
         OC    JOBREPNO,JOBREPNO   REPORT # FILTER                              
         BZ    *+14                                                             
         CLC   JOBREPNO,TBJPQSEQ                                                
         BNE   BLJ22                                                            
*                                                                               
         MVC   BYTE,TBJADV                                                      
         NI    BYTE,X'0F'                                                       
         OC    JOBADV,JOBADV       ADV FILTER                                   
         BZ    BLJ14                                                            
         CLC   JOBADV,BYTE                                                      
         BNE   BLJ22                                                            
         B     BLJ16                                                            
*                                                                               
BLJ14    TM    SYSIDFL,FACITST     TST SYSTEMS?                                 
         BO    BLJ16               YES                                          
*                                                                               
         XR    RE,RE                                                            
         IC    RE,BYTE                                                          
         MHI   RE,L'FACITAB                                                     
         L     RF,AFID                                                          
         AR    RF,RE                                                            
         USING FACITABD,RF                                                      
*                                                                               
         TM    SYSIDFL,FACIREP                                                  
         BO    BLJ15                                                            
         TM    FACIFL,FACIREP                                                   
         BO    BLJ22               SYSID=ADV, JOB SYSID=REP -- SKIP             
         CLI   FACISN4,C' '        ANY FACPAK NAME?                             
         BE    BLJ22               NONE - SKIP IT.                              
         B     BLJ16               SYSID=ADV, JOB SYSID=ADV                     
*                                                                               
BLJ15    TM    FACIFL,FACIREP                                                   
         BZ    BLJ22               SYSID=REP, JOB SYSID=ADV -- SKIP             
         DROP  RF                  SYSID=REP, JOB SYSID=REP                     
*                                                                               
BLJ16    OC    JOBMONS,JOBMONS     MONSOON FILTER                               
         BZ    *+14                                                             
         CLC   JOBMONS,TBJMONS                                                  
         BNE   BLJ22                                                            
*                                                                               
         CLI   UPDS,YES            ONLY UPDATIVE SOON                           
         BNE   BLJ17                                                            
         TM    TBJSTAT,TBJUPDT                                                  
         BZ    BLJ22                                                            
*                                                                               
BLJ17    DS    0H                                                               
*&&US                                                                           
         CLI   LONGS,C'A'          ALL                                          
         BE    BLJ18                                                            
         CLC   LONGS,TBJCTYPE                                                   
         BNE   BLJ22                                                            
*&&                                                                             
*&&UK                                                                           
         CLI   LONGS,C'R'          ONLY REGULAR                                 
         BNE   *+12                                                             
         TM    TBJSTAT,TBJLONG                                                  
         BO    BLJ22                                                            
*                                                                               
         CLI   LONGS,JCLONG        ONLY LONGS                                   
         BNE   *+12                                                             
         TM    TBJSTAT,TBJLONG                                                  
         BZ    BLJ22                                                            
*&&                                                                             
BLJ18    MVC   B.TBJNTRY,TBJNTRY   SAVE ENTRY IN JOBTAB                         
         LR    R1,R2                                                            
         S     R1,AJOBTAB                                                       
         SRL   R1,6                                                             
         STCM  R1,15,B.TBJOFS      SAVE OFFSET TO REAL ENTRY                    
*                                                                               
         XR    RF,RF               SUBMITTED TIME                               
         ICM   RF,7,B.TBJSTIME                                                  
         CLI   B.TBJSTIME,X'FF'    TIME REBUILT BY RERUN?                       
         BNE   BLJ20                                                            
*                                                                               
         N     RF,=X'0000FFFF'     TIME *3/4                                    
         XR    RE,RE               USES BOBBY'S FUNKY 3/4S INTERVALS            
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
         STCM  RF,7,B.TBJSTIME                                                  
*                                                                               
BLJ20    MVI   B.TBJSTAT,JOBFSUB  QUICK STATUS SET                              
         OC    TBJRTIME,TBJRTIME                                                
         BZ    *+8                                                              
         MVI   B.TBJSTAT,JOBFRUN                                                
         OC    TBJETIME,TBJETIME                                                
         BZ    *+8                                                              
         MVI   B.TBJSTAT,JOBFAVA                                                
         TM    TBJSTAT,TBJUPDT                                                  
         BZ    *+8                                                              
         OI    B.TBJSTAT,JOBFUPDT                                               
         TM    TBJSTAT,TBJLONG                                                  
         BZ    *+8                                                              
         OI    B.TBJSTAT,JOBFLONG                                               
         TM    TBJSTAT,TBJKILL                                                  
         BZ    *+12                                                             
         MVI   B.TBJSTAT,JOBFKILL                                               
         B     BLJ21                                                            
         TM    TBJSTAT,TBJERROR                                                 
         BZ    *+12                                                             
         MVI   B.TBJSTAT,JOBFERR                                                
         MVI   TBJSTAT,0                                                        
         TM    TBJSTAT,TBJHOLD                                                  
         BZ    *+8                                                              
         MVI   B.TBJSTAT,JOBFHOLD                                               
         TM    TBJSTAT,TBJIGNOR                                                 
         BZ    *+8                                                              
         OI    B.TBJSTAT,TBJIGNOR                                               
*        TM    TBJSTAT,TBJRUNOW                                                 
*        BZ    *+8                                                              
*        OI    B.TBJSTAT,TBJRUNOW                                               
*                                                                               
BLJ21    AHI   R3,1                                                             
         AH    R4,JOBTABL          NEXT ENTRY                                   
*                                                                               
         L     RF,MYSSB                                                         
         ICM   R0,15,SSBTSAR-SSBD(RF)                                           
*                                                                               
         LR    RF,R4               MAKE SURE THERE IS SPACE FOR 1 MORE          
         AH    RF,JOBTABL                                                       
         S     RF,ABIGBUF                                                       
         CR    RF,R0                                                            
         BL    BLJ24                                                            
*                                                                               
BLJ22    AH    R2,JOBTABL                                                       
         C     R2,JT.DSPTEND       DON'T RUN OFF TABLE END                      
         BL    BLJ02                                                            
*                                                                               
BLJ24    ST    R3,TABCOUNT                                                      
         BRAS  RE,ARSOFF                                                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+10(2),JOBTABL                                               
         ICM   R0,15,ABIGBUF                                                    
         ICM   R2,15,TABCOUNT                                                   
         BZ    EXITOK                                                           
*                                                                               
         GOTO1 VQSORT,DMCB,(R0),(R2),,L'TBJSTIME,TBJSTIME-TBJNTRY               
         B     EXITOK                                                           
         DROP  B,R2                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO RESET QUEUE                                              *         
***********************************************************************         
RESET    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTFRST                                                
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         MVC   TBJTIME,EFFS        FLAG EMERGENCY REBUILD                       
         SAC   0                                                                
         B     DONE                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SUMMARY SCREEN                                   *         
***********************************************************************         
SUMMARY  NTR1  BASE=*,LABEL=*                                                   
         XC    SEQS,SEQS           CLEAR DISPLAY SEQUENCE NUMBERS               
         XC    TOTSUB,TOTSUB                                                    
         XC    TOTRUN,TOTRUN                                                    
         XC    AVETIME,AVETIME                                                  
*                                                                               
         BRAS  RE,DSHDR            OUTPUT DISPLAY HEADER                        
*                                                                               
         MVI   DISSD,NO            SET WANT TO BUILD BY CLASS ONLY              
*                                                                               
         LA    RF,BLDNSUM                                                       
         CLI   UPDS,YES            UPDATIVE SOON FILTER                         
         BE    SUMX01                                                           
         OC    JOBADV,JOBADV                                                    
         BZ    *+8                                                              
SUMX01   LA    RF,BLDSUM                                                        
         BASR  RE,RF               BUILD SUMMARY DATA IN TSAR BUFFER            
*                                                                               
         SAM31                                                                  
         L     R2,ABIGBUF                                                       
         USING CLASTABD,R2                                                      
SUMX02   OC    CLASCLAS,CLASCLAS   END OF TABLE                                 
         BZ    SUMX04                                                           
         ICM   RF,15,CLASWAIT      ADD TO TOTAL AVERAGE WAIT TIME               
         BNP   *+12                                                             
         A     RF,AVETIME                                                       
         ST    RF,AVETIME                                                       
         AHI   R2,CLASTABL         NEXT ENTRY                                   
         B     SUMX02                                                           
*                                                                               
SUMX04   BRAS  RE,SCROLL                                                        
*                                                                               
         L     R2,ABIGBUF                                                       
         USING CLASTABD,R2                                                      
         LA    R3,RUNLINH          FIRST LINE                                   
         USING SUMLINED,R3                                                      
*                                                                               
SUMM02   OC    CLASCLAS,CLASCLAS   END OF TABLE                                 
         BZ    SUMX                                                             
         LA    RF,RUNXXXH          END OF SCREEN                                
         CR    RF,R3                                                            
         BNH   SUMX                                                             
*                                                                               
         LH    RF,SEQNUM           BUMP SEQUENCE NUMBER                         
         AHI   RF,1                                                             
         STH   RF,SEQNUM                                                        
         CH    RF,STARTPF          WHERE TO START FOR PF SCROLLING              
         BNL   *+12                                                             
         AHI   R2,CLASTABL         NEXT ENTRY                                   
         B     SUMM02                                                           
*                                                                               
         MVC   SUMLCLS,CLASCLAS                                                 
         MVC   SUMLSML,CLASSML                                                  
         EDIT  (B2,CLASNSUB),(3,SUMLSUB)                                        
         EDIT  (B2,CLASTSUB),(5,SUMTSUB)                                        
         EDIT  (B2,CLASNRUN),(3,SUMLRUN)                                        
         EDIT  (B2,CLASTRUN),(5,SUMTRUN)                                        
         EDIT  (B2,CLASTRDY),(5,SUMTRDY)                                        
*                                                                               
         CLC   CLASSUBT,EFFS       SUBMIT TIME                                  
         BE    SUMM03              NOTHING SUBMITTED                            
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,7,CLASSUBT       TIME OF OLDEST SUBMITTED                     
         BZ    SUMM03                                                           
         L     RF,CURTIME                                                       
         SR    RF,R0                                                            
         BNP   SUMM03              NO WAIT TIME PERIOD                          
*                                                                               
         BRAS  RE,SHOWWAIT                                                      
         MVC   SUMSUBT,WORK2                                                    
*                                                                               
SUMM03   CLC   CLASRUNT,EFFS       RUN TIME SET?                                
         BE    SUMM04                                                           
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,7,CLASRUNT       TIME OF OLDEST RUNNING                       
         BZ    SUMM04                                                           
*                                                                               
         L     RF,CURTIME          SHOW LENGTH OF TIME THE OLDEST JOB           
         SR    RF,R0               HAS BEEN RUNNING                             
         BNP   SUMM04                                                           
         BRAS  RE,SHOWWAIT                                                      
         MVC   SUMRUNW,WORK2                                                    
*                                                                               
SUMM04   ICM   RF,15,CLASWAIT      ADD TO TOTAL AVERAGE WAIT TIME               
         BNP   SUMM06                                                           
         XR    RE,RE               DIVIDE TOTAL WAIT BY # SUBMITTED             
         XR    R0,R0                                                            
         ICM   R0,3,CLASNSUB       TO GET AVERAGE                               
         BZ    SUMM06                                                           
         DR    RE,R0                                                            
         BRAS  RE,SHOWWAIT         OUTPUT AVERAGE AS NH MM OR MM:SS             
         MVC   SUMWAIT,WORK2                                                    
*                                                                               
SUMM06   ICM   RF,15,CLASWTR       SET AVERAGE RUN TIME                         
         BNP   SUMM10                                                           
         XR    RE,RE               DIVIDE TOTAL WAIT BY # SUBMITTED             
         XR    R0,R0                                                            
         ICM   R0,3,CLASNRUN       TO GET AVERAGE                               
         BZ    SUMM10                                                           
         DR    RE,R0                                                            
         BRAS  RE,SHOWWAIT         OUTPUT AVERAGE AS NH MM OR MM:SS             
         MVC   SUMRUNT,WORK2                                                    
*                                                                               
SUMM10   XR    RF,RF               GO TO NEXT LINE                              
         IC    RF,0(R3)                                                         
         AR    R3,RF                                                            
         AHI   R2,CLASTABL         GO TO NEXT CLASS TABLE ENTRY                 
*                                                                               
         OC    SEQLO,SEQLO         SET DISPLAY SEQUENCE NUMBERS                 
         BNZ   *+10                                                             
         MVC   SEQLO,SEQNUM                                                     
         MVC   SEQHI,SEQNUM                                                     
         B     SUMM02                                                           
         DROP  R2,R3                                                            
*                                                                               
* FORMAT MESSAGE                                                                
*                                                                               
SUMX     LA    R1,RUNACTH          SET FADR FOR ERROR/EXIT                      
         ST    R1,FADR                                                          
         XC    FLD(L'RUNMSG),FLD   FORMAT MESSAGE IN FLD FOR EXIT               
         MVC   FLD(L'SUMMSG),SUMMSG                                             
         LA    R7,FLD                                                           
         LA    R7,L'SUMMSG-4(R7)                                                
         EDIT  (B2,TOTSUB),(4,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                    
*                                                                               
         AR    R7,R0                                                            
         MVC   0(L'SUMMSG0,R7),SUMMSG0                                          
         LA    R7,L'SUMMSG0-4(R7)                                               
         EDIT  (B2,TOTRUN),(4,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                    
*                                                                               
         AR    R7,R0                                                            
         MVC   0(L'SUMMSG1,R7),SUMMSG1                                          
         LA    R7,L'SUMMSG1-5(R7)                                               
         OC    AVETIME,AVETIME                                                  
         BZ    SUMMX30                                                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RF,15,AVETIME                                                    
         XR    R0,R0                                                            
         ICM   R0,3,TOTSUB                                                      
         BZ    SUMMX30                                                          
         DR    RE,R0                                                            
         BRAS  RE,SHOWWAIT         OUTPUT AVERAGE WAIT                          
         MVC   0(5,R7),WORK2                                                    
*                                                                               
SUMMX30  DS    0H                                                               
         LA    R7,5(R7)                                                         
         OC    TOTSUBB,TOTSUBB     ANY TYPE B                                   
         BZ    SUM900                                                           
         MVC   0(L'SUMMSG2,R7),SUMMSG2                                          
         LA    R7,L'SUMMSG2-4(R7)                                               
         EDIT  (B2,TOTSUBB),(4,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                   
         AR    R7,R0                                                            
         MVC   0(L'SUMMSG3,R7),SUMMSG3                                          
         LA    R7,L'SUMMSG3-5(R7)                                               
*                                                                               
         OC    AVETIMEB,AVETIMEB   R1 = TIME IN SECONDS ONLY                    
         BZ    SUM900                                                           
         L     R1,AVETIMEB                                                      
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),TOTSUB                                                 
         SR    R0,R0                                                            
         D     R0,FULL                                                          
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R7),DUB                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R7),DUB                                                      
         LA    R7,5(R7)                                                         
*                                                                               
SUM900   MVC   0(L'SUMMSG4,R7),SUMMSG4                                          
         LA    R7,L'SUMMSG4-3(R7)                                               
         EDIT  (B4,TABCOUNT),(3,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                  
*                                                                               
         MVC   RUNMSG,FLD                                                       
         MVC   RUNSLO,SEQLO        PUT IN S/R SAVED STORAGE                     
         MVC   RUNSHI,SEQHI                                                     
         MVC   RUNSNUM,SEQNUM                                                   
*                                                                               
         ICM   R0,15,TOTJOB                                                     
         BZ    DONE                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
         LA    R3,RUNHEDH          FIRST LINE                                   
         USING SUMLINED,R3                                                      
         MVC   SUMLRDY(4),=CL4'RDY='                                            
         UNPK  SUMLRDY+4(5),DUB                                                 
         B     DONE                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE SCROLLING                                         *         
* NTRY: TABCOUNT SET CORRECTLY                                        *         
***********************************************************************         
SCROLL   NTR1  BASE=*,LABEL=*                                                   
         XC    STARTPF,STARTPF                                                  
         CLC   RUNSACT,ACT.ACTACT  SAME ACTION AS BEFORE?                       
         BNE   SCR06                                                            
*                                                                               
         LH    RE,RUNSTNUM         NEXT TRANSACTION FOR THIS TERMINAL?          
         AHI   RE,1                                                             
         CH    RE,TRANSNUM                                                      
         BNE   SCR06                                                            
*                                                                               
         CLI   PFKEY,0             ENTER = REFRESH                              
         BNE   *+14                                                             
         MVC   STARTPF,RUNSLO                                                   
         B     SCR06                                                            
*                                                                               
         CLI   PFKEY,7             PAGE UP                                      
         BNE   SCR02                                                            
         LH    R1,RUNSLO                                                        
         AHI   R1,-(RUNSMAXN)                                                   
         BM    SCR06                                                            
         STH   R1,STARTPF                                                       
         B     SCR06                                                            
*                                                                               
SCR02    CLI   PFKEY,8             PAGE DOWN                                    
         BNE   SCR04                                                            
         LH    R1,RUNSHI                                                        
         AHI   R1,1                                                             
         STH   R1,STARTPF                                                       
         B     SCR06                                                            
*                                                                               
SCR04    CLI   PFKEY,5             TOP                                          
         BE    SCR06                                                            
*                                                                               
         CLI   PFKEY,6             BOTTOM                                       
         BNE   SCR06                                                            
         L     R1,TABCOUNT                                                      
         AHI   R1,-(RUNSMAXN-1)                                                 
         BM    SCR06                                                            
         STH   R1,STARTPF                                                       
         B     SCR06                                                            
*                                                                               
SCR06    CLC   STARTPF,TABCOUNT+2  MORE THAN IN TABLE?                          
         BNH   *+10                                                             
         XC    STARTPF,STARTPF     THEN START AT TOP                            
         B     EXITOK                                                           
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO BUILD SUMMARY SCREEN DISPLAY                             *         
***********************************************************************         
BLDSUM   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VGETFACT,DMCB,(X'01',0)                                          
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         ICM   R0,15,FATIME                                                     
         MHI   R0,100              TIME BIN PLEASE                              
         A     R0,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         ST    R0,CURTIME          CURRENT BINARY TIME                          
         DROP  R1                                                               
*                                                                               
         BRAS  RE,ARSOFF                                                        
         SAM31                                                                  
         L     RF,MYSSB                                                         
         ICM   R1,15,SSBTSAR-SSBD(RF)                                           
         L     R0,ABIGBUF                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR TSAR BUFFER                            
*                                                                               
         XC    TABCOUNT,TABCOUNT   CLEAR COUNT OF ENTRIES IN TSAR TABLE         
         XC    TOTSUB,TOTSUB                                                    
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTFRST                                                
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         AH    R2,TBJLHEAD                                                      
         USING TBJOBTAB,R2         R2 = TABS DATASPACE JOB TABLE                
*                                                                               
         XR    R3,R3               R3 = COUNT OF COPIED CLASSES                 
SBLD02   OC    TBJCLASS(4),TBJCLASS                                             
         BZ    SBLD04              IGNORE EMPTIES                               
         CLC   TBJCLASS(4),EFFS                                                 
         BE    SBLD04              IGNORE TEMP                                  
         CLI   TBJSTAT,0                                                        
         BE    SBLD04              IGNORE FLAGGED TO DELETE                     
*                                                                               
         CLI   UPDS,YES            ONLY UPDATIVE SOON                           
         BNE   *+12                                                             
         TM    TBJSTAT,TBJUPDT                                                  
         BZ    SBLD04                                                           
*                                                                               
         OC    JOBADV,JOBADV       ADV FILTER                                   
         BZ    SBLD06                                                           
         MVC   BYTE,TBJADV                                                      
         NI    BYTE,X'0F'                                                       
         CLC   JOBADV,BYTE                                                      
         BNE   SBLD04                                                           
*                                                                               
         B     SBLD06                                                           
*                                                                               
SBLD04   AH    R2,JOBTABL          NEXT ENTRY                                   
         CLM   R2,15,JT.DSPTEND                                                 
         BL    SBLD02                                                           
         B     SBLD20                                                           
*                                                                               
SBLD06   L     R4,ABIGBUF                                                       
         USING CLASTABD,R4         R4 = TSAR CLASS TABLE                        
*                                                                               
SBLD08   OC    CLASCLAS,CLASCLAS   EOT?                                         
         BNZ   *+12                                                             
         AHI   R3,1                UNIQUE ENTRIES                               
         B     SBLD12                                                           
*                                                                               
         CLC   CLASCLAS,TBJCLASS   ALREADY HAVE THIS CLASS?                     
         BNE   SBLD10              YES                                          
         CLI   DISSD,YES           BREAK OUT BY AGENCY?                         
         BNE   SBLD12              NO                                           
         CLC   CLASAGY,TBJAGY      MATCH AGENCY?                                
         BNE   SBLD10              NO                                           
         B     SBLD12                                                           
*                                                                               
SBLD10   AHI   R4,CLASTABL                                                      
         B     SBLD08                                                           
*                                                                               
SBLD12   MVC   CLASCLAS,TBJCLASS                                                
         CLI   DISSD,YES           TREAT ALL AS UNIQUE?                         
         BNE   *+10                NO                                           
         MVC   CLASAGY,TBJAGY                                                   
         OC    CLASSUBT,CLASSUBT   SET FIRST TIME THROUGH                       
         BNZ   *+10                                                             
         MVC   CLASSUBT,EFFS                                                    
         OC    CLASRUNT,CLASRUNT   SET FIRST TIME THROUGH                       
         BNZ   *+10                                                             
         MVC   CLASRUNT,EFFS                                                    
*                                                                               
         OC    TBJETIME,TBJETIME   READY?                                       
         BZ    SBLD14              NO                                           
         LH    R0,CLASNRDY                                                      
         AHI   R0,1                                                             
         STH   R0,CLASNRDY         # READY NOW                                  
         B     SBLD04              NEXT ENTRY                                   
*                                                                               
SBLD14   OC    TBJRTIME,TBJRTIME   RUNNING?                                     
         BZ    SBLD16              NO                                           
         LH    R0,CLASNRUN                                                      
         AHI   R0,1                                                             
         STH   R0,CLASNRUN         # RUNNING NOW                                
         LH    R0,TOTRUN                                                        
         AHI   R0,1                                                             
         STH   R0,TOTRUN           TOTAL RUNNING                                
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,TBJRTIME                                                    
         A     RF,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         CLM   RF,7,CLASRUNT       SET TIME OF OLDEST RUNNING                   
         BH    *+8                                                              
         STCM  RF,7,CLASRUNT                                                    
*                                                                               
         ICM   R0,15,CURTIME       ADJUST TOTAL WAIT TIME                       
         SR    R0,RF                                                            
         BP    *+6                                                              
         XR    R0,R0               SUBMITTED AFTER START OF SEARCH              
         ICM   RF,15,CLASWTR                                                    
         AR    RF,R0                                                            
         STCM  RF,15,CLASWTR                                                    
         B     SBLD04                                                           
*                                                                               
SBLD16   LH    R0,CLASNSUB                                                      
         AHI   R0,1                                                             
         STH   R0,CLASNSUB         # SUBMITTED NOW                              
         LH    R0,TOTSUB                                                        
         AHI   R0,1                                                             
         STH   R0,TOTSUB           TOTAL SUBMITTED                              
*                                                                               
         XR    RF,RF               SUBMITTED TIME                               
         ICM   RF,7,TBJSTIME                                                    
         CLI   TBJSTIME,X'FF'      TIME REBUILT BY RERUN?                       
         BNE   SBLD18                                                           
*                                                                               
         N     RF,=X'0000FFFF'     TIME *3/4                                    
         XR    RE,RE               USES BOBBY'S FUNKY 3/4S INTERVALS            
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
*                                                                               
SBLD18   A     RF,DDSTIME          CORRECTION FACTOR FOR DDS TIME               
         CLM   RF,7,CLASSUBT       SET OLDEST SUBMITTED                         
         BH    *+8                                                              
         STCM  RF,7,CLASSUBT                                                    
*                                                                               
         ICM   R0,15,CURTIME       ADJUST TOTAL WAIT TIME                       
         SR    R0,RF                                                            
         BP    *+6                                                              
         XR    R0,R0               SUBMITTED AFTER START OF SEARCH              
         ICM   RF,15,CLASWAIT                                                   
         AR    RF,R0                                                            
         STCM  RF,15,CLASWAIT                                                   
         B     SBLD04                                                           
         DROP  R2,R4                                                            
*                                                                               
SBLD20   BRAS  RE,ARSOFF           FINISHED WITH ALL ENTRIES                    
         ST    R3,TABCOUNT         SAVE # OF ENTRIES                            
*                                                                               
         ICM   R2,15,TABCOUNT      NOW SORT THE CLASSES BY # SUB                
         BZ    EXITOK                                                           
*                                                                               
         ICM   R0,15,ABIGBUF                                                    
*                                                                               
         CLI   USETIME,YES         SORT BY OLDEST                               
         BE    SBLD22                                                           
         CLI   USERUN,YES          SORT BY RUNNING                              
         BE    SBLD24                                                           
*                                                                               
         O     R0,=XL4'80000000'   DESCENDING SORT                              
         GOTO1 VQSORT,DMCB,(R0),(R2),CLASTABL,                         +        
               L'CLASNSUB,CLASNSUB-CLASTABD                                     
         B     EXITOK                                                           
*                                                                               
SBLD22   GOTO1 VQSORT,DMCB,(R0),(R2),CLASTABL,                         +        
               L'CLASSUBT,CLASSUBT-CLASTABD                                     
         B     EXITOK                                                           
*                                                                               
SBLD24   O     R0,=XL4'80000000'   DESCENDING SORT                              
         GOTO1 VQSORT,DMCB,(R0),(R2),CLASTABL,                         +        
               L'CLASNRUN,CLASNRUN-CLASTABD                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD NEW SUMMARY LIST DISPLAY FROM CLASS TABLE          *         
***********************************************************************         
BLDNSUM  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VGETFACT,DMCB,(X'01',0)                                          
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         ICM   R0,15,FATIME                                                     
         MHI   R0,100              TIME BIN PLEASE                              
         A     R0,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         ST    R0,CURTIME          CURRENT BINARY TIME                          
         DROP  R1                                                               
*                                                                               
         BRAS  RE,ARSOFF                                                        
         SAM31                                                                  
         L     RF,MYSSB                                                         
         ICM   R1,15,SSBTSAR-SSBD(RF)                                           
         L     R0,ABIGBUF                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    TOTJOB,TOTJOB                                                    
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,CL.DSPTFRST                                                
         SAC   512                                                              
         USING JCLASSD,R2          R2 = TABS DATASPACE CLASS TABLE              
*                                                                               
         XR    R3,R3               R3 = COUNT OF COPIED CLASSES                 
         L     R4,ABIGBUF                                                       
         USING CLASTABD,R4         R4 = TSAR CLASS TABLE                        
*                                                                               
SDM02    OC    JCLASSD(JCKEYL),JCLASSD                                          
         BZ    SDM20                                                            
*                                                                               
SDM02L   DS    0H                                                               
*&&US                                                                           
         CLI   LONGS,C'A'                                                       
         BE    SDM02U                                                           
         CLC   LONGS,JCTYPE                                                     
         BE    SDM02U                                                           
         AHI   R2,JCLASSL          NEXT CLASS TABLE ENTRY                       
         B     SDM02                                                            
*&&                                                                             
*&&UK                                                                           
         CLI   LONGS,JCLONG        ONLY LONGS                                   
         BNE   SDM02R                                                           
         CLI   JCTYPE,JCLONG                                                    
         BE    SDM02U                                                           
         AHI   R2,JCLASSL          NEXT CLASS TABLE ENTRY                       
         B     SDM02                                                            
                                                                                
SDM02R   DS    0H                                                               
*&&UK*&& CLI   LONGS,C'R'                                                       
         BNE   SDM02U                                                           
         CLI   JCTYPE,JCLONG                                                    
         BNE   SDM02U                                                           
         AHI   R2,JCLASSL          NEXT CLASS TABLE ENTRY                       
         B     SDM02                                                            
*&&                                                                             
SDM02U   CLI   USETIME,YES         SORT BY OLDEST                               
         BNE   SDM03                                                            
         OC    JCNSUB,JCNSUB       KEEP ONLY THOSE WITH SUB JOBS                
         BNZ   SDM03                                                            
         AHI   R2,JCLASSL          NEXT CLASS TABLE ENTRY                       
         B     SDM02                                                            
*                                                                               
SDM03    CLI   DISSD,YES           TREAT ALL AS UNIQUE?                         
         BE    SDM06               YES                                          
*                                                                               
         L     R4,ABIGBUF                                                       
         USING CLASTABD,R4         R4 = TSAR CLASS TABLE                        
SDM04    OC    CLASCLAS,CLASCLAS   EOT?                                         
         BNZ   *+12                NO                                           
         AHI   R3,1                ONLY UNIQUE VALUES                           
         B     SDM06                                                            
*                                                                               
         CLC   CLASCLAS,JCCLASS    ALREADY HAVE THIS CLASS?                     
         BNE   *+14                YES                                          
         CLC   JCTYPE,CLASSML      MATCH TYPE?                                  
         BE    SDM06                                                            
*                                                                               
SDM05    AHI   R4,CLASTABL                                                      
         B     SDM04                                                            
*                                                                               
SDM06    MVC   CLASCLAS,JCCLASS    CLASS                                        
         MVC   CLASSML(1),JCTYPE                                                
         CLI   DISSD,YES           TREAT ALL AS UNIQUE?                         
         BNE   *+10                NO                                           
         MVC   CLASAGY,JCAGY                                                    
*                                                                               
         LH    R0,CLASNSUB                                                      
         AH    R0,JCNSUB                                                        
         STH   R0,CLASNSUB         # SUBMITTED NOW                              
         LH    R0,CLASTSUB                                                      
         AH    R0,JCNTSUB                                                       
         STH   R0,CLASTSUB         # SUBMITTED TODAY                            
         LH    R0,TOTSUB                                                        
         AH    R0,JCNSUB                                                        
         STH   R0,TOTSUB           # SUBMITTED FOR ALL TODAY                    
*                                                                               
         LH    R0,CLASNRUN                                                      
         AH    R0,JCNRUN                                                        
         STH   R0,CLASNRUN         # RUNNING NOW                                
         LH    R0,CLASTRUN                                                      
         AH    R0,JCNTRUN                                                       
         STH   R0,CLASTRUN         # RUN TODAY                                  
         LH    R0,TOTRUN                                                        
         AH    R0,JCNRUN                                                        
         STH   R0,TOTRUN           TOTAL # RUNNING NOW                          
*                                                                               
         LH    R0,CLASNRDY                                                      
         AH    R0,JCNRDY                                                        
*??      STH   R0,CLASNRDY         # READY NOW                                  
         LH    R0,CLASTRDY                                                      
         AH    R0,JCNTRDY                                                       
         STH   R0,CLASTRDY         # READY TODAY                                
*                                                                               
         L     R0,TOTJOB                                                        
         AH    R0,JCNTRDY                                                       
         ST    R0,TOTJOB                                                        
*                                                                               
         ICM   R5,15,JCFSUB        FIRST SUBMITTED JOB FOR THIS CLASS           
         BZ    SDM12                                                            
         CPYA  AR5,AR2                                                          
*                                                                               
         USING TBJOBTAB,R5                                                      
SDM08    XR    RF,RF               SUBMITTED TIME                               
         ICM   RF,7,TBJSTIME                                                    
         CLI   TBJSTIME,X'FF'      TIME REBUILT BY RERUN?                       
         BNE   SDM10                                                            
*                                                                               
         N     RF,=X'0000FFFF'     TIME *3/4                                    
         XR    RE,RE               USES BOBBY'S FUNKY 3/4S INTERVALS            
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
*                                                                               
SDM10    A     RF,DDSTIME          CORRECTION FACTOR FOR DDS TIME               
         OC    CLASSUBT,CLASSUBT   SET FIRST TIME THROUGH                       
         BNZ   *+8                                                              
         STCM  RF,7,CLASSUBT                                                    
*                                                                               
         CLM   RF,7,CLASSUBT       THIS ONE IS OLDER?                           
         BH    *+8                 NO                                           
         STCM  RF,7,CLASSUBT                                                    
*                                                                               
         ICM   R0,15,CURTIME       ADJUST TOTAL WAIT TIME                       
         SR    R0,RF                                                            
         ICM   RF,15,CLASWAIT                                                   
         AR    RF,R0                                                            
         STCM  RF,15,CLASWAIT                                                   
*                                                                               
         ICM   R5,15,TBJNXT        NEXT IN SUBMITTED QUEUE                      
         BNZ   SDM08                                                            
*                                                                               
SDM12    ICM   R5,15,JCFRUN        NOW PROCESS RUN QUEUE                        
         BZ    SDM16                                                            
         CPYA  AR5,AR2                                                          
         USING TBJOBTAB,R5                                                      
*                                                                               
         OC    CLASRUNT,CLASRUNT   FIRST TIME THOUGH?                           
         BNZ   SDM14               NO                                           
         XR    RF,RF                                                            
         ICM   RF,7,TBJRTIME                                                    
         A     RF,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         STCM  RF,7,CLASRUNT                                                    
*                                                                               
SDM14    XR    RF,RF                                                            
         ICM   RF,7,TBJRTIME                                                    
         A     RF,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         CLM   RF,7,CLASRUNT                                                    
         BH    *+8                                                              
         STCM  RF,7,CLASRUNT       SET TIME OF OLDEST RUNNING                   
*                                                                               
         ICM   R0,15,CURTIME       ADJUST TOTAL WAIT TIME                       
         SR    R0,RF                                                            
         BP    *+6                                                              
         XR    R0,R0               SUBMITTED AFTER START OF SEARCH              
         ICM   RF,15,CLASWTR                                                    
         AR    RF,R0                                                            
         STCM  RF,15,CLASWTR                                                    
*                                                                               
         ICM   R5,15,TBJNXT        NEXT IN RUN QUEUE                            
         BNZ   SDM14                                                            
*                                                                               
SDM16    LAM   AR5,AR5,ARZERO                                                   
         DROP  R5                                                               
*                                                                               
SDM18    AHI   R2,JCLASSL          NEXT CLASS TABLE ENTRY                       
         CLI   DISSD,YES           TREAT ALL UNIQUE?                            
         BNE   SDM02               NO                                           
         AHI   R3,1                                                             
         AHI   R4,CLASTABL         NEXT IN TSAR BUFFER                          
         B     SDM02               AND CONTINUE                                 
         DROP  R2,R4                                                            
*                                                                               
SDM20    BRAS  RE,ARSOFF           FINISHED WITH ALL ENTRIES                    
         ST    R3,TABCOUNT         SAVE # OF ENTRIES                            
*                                                                               
         ICM   R2,15,TABCOUNT                                                   
         BZ    EXITOK                                                           
         ICM   R0,15,ABIGBUF                                                    
*                                                                               
         CLI   USETIME,YES         SORT BY OLDEST                               
         BE    SDM22                                                            
         CLI   USERUN,YES          SORT BY RUNNING                              
         BE    SDM24                                                            
         TM    JOBFILT,JOBFAVA                                                  
         BO    SDM26                                                            
*                                                                               
         O     R0,=XL4'80000000'   DESCENDING SORT                              
         GOTO1 VQSORT,DMCB,(R0),(R2),CLASTABL,                         +        
               L'CLASNSUB+L'CLASSUBT,CLASNSUB-CLASTABD                          
         B     EXITOK                                                           
*                                                                               
SDM22    GOTO1 VQSORT,DMCB,(R0),(R2),CLASTABL,                         +        
               L'CLASSUBT,CLASSUBT-CLASTABD                                     
         B     EXITOK                                                           
*                                                                               
SDM24    O     R0,=XL4'80000000'   DESCENDING SORT                              
         GOTO1 VQSORT,DMCB,(R0),(R2),CLASTABL,                         +        
               L'CLASNRUN+L'CLASRUNT,CLASNRUN-CLASTABD                          
         B     EXITOK                                                           
*                                                                               
SDM26    O     R0,=XL4'80000000'   DESCENDING SORT                              
         GOTO1 VQSORT,DMCB,(R0),(R2),CLASTABL,                         +        
               L'CLASTRDY,CLASTRDY-CLASTABD                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SUMMARY SCREEN                                   *         
***********************************************************************         
SUKMARY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SUKCHR,0                                                         
         BNE   *+8                                                              
         MVI   SUKCHR,C'C'                                                      
         XC    TABCOUNT,TABCOUNT                                                
*                                                                               
         SAM31                                                                  
         L     R3,ABIGBUF          CLEAR FIRST ENTRY                            
         XC    0(32,R3),0(R3)                                                   
*                                                                               
SUK010   SAC   512                                                              
         LAM   AR2,AR2,TBLET         KEEP TAKING THE TABLETS                    
         L     R2,AJOBTAB                                                       
         USING TBJOBTAB,R2         R2=A(JOB SCHEDULER TABLE)                    
         ICM   R0,15,0(R2)                                                      
         BZ    SUKX                                                             
         ST    R0,NUMJOBS                                                       
         AH    R2,JOBHDRL          POINT TO FIRST ENTRY                         
*                                                                               
SUK020   L     R3,ABIGBUF                                                       
         OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BZ    SUK040                                                           
         SR    RE,RE                                                            
         ICM   RE,7,TBJSTIME       TEST ANY VALID DATA                          
         BZ    SUK050                                                           
*                                                                               
         CLI   SUKCHR,C'L'         LONGWAIT                                     
         BNE   SUK021                                                           
         ICM   RF,15,TBJETIME      IGNORE ENDED                                 
         BNZ   SUK050                                                           
         ICM   RF,15,TBJRTIME      IGNORE RUNNING                               
         BNZ   SUK050                                                           
         ICM   RF,15,CURTIME                                                    
         SR    RF,RE               RE=WAIT TIME IN 100TH SEC                    
         C     RF,=AL4(30*60*100)                                               
         BL    SUK050              IGNORE IF LESS THAN 30 MINS WAIT             
*                                                                               
SUK021   OC    JOBCLS,JOBCLS       CLASS FILTER                                 
         BZ    *+14                                                             
         CLC   TBJCLASS(2),JOBCLS                                               
         BNE   SUK050                                                           
*                                                                               
SUK025   XC    DUB,DUB                                                          
         XC    DUB1,DUB1           SAVE HEADER TEXT IN DUB1                     
*                                                                               
         CLI   SUKCHR,C'C'                                                      
         BNE   *+16                                                             
         MVC   DUB(2),TBJCLASS     SET CLASS SUMMARY                            
         MVC   DUB1,=C'CLASS   '                                                
*                                                                               
         CLI   SUKCHR,C'T'                                                      
         BNE   *+16                                                             
         MVC   DUB,TBJLUID         SET LUID SUMMARY                             
         MVC   DUB1,=C'TERMINAL'                                                
*                                                                               
         CLI   SUKCHR,C'J'                                                      
         BNE   *+16                                                             
         MVC   DUB,TBJMVSID        SET MVSID SUMMARY                            
         MVC   DUB1,=C'MVSID   '                                                
*                                                                               
         CLI   SUKCHR,C'L'                                                      
         BNE   *+16                                                             
         MVC   DUB,TBJMVSID        SET MVSID LONG WAIT                          
         MVC   DUB1,=C'MVSID   '                                                
*                                                                               
         CLI   SUKCHR,C'U'                                                      
         BNE   *+16                                                             
         MVC   DUB(2),TBJPQUSR     SET USERID SUMMARY                           
         MVC   DUB1,=C'USERID  '                                                
*                                                                               
         CLI   SUKCHR,C'A'                                                      
         BNE   *+16                                                             
         MVC   DUB(1),TBJADV       SET ADV SUMMARY                              
         MVC   DUB1,=C'ADV     '                                                
*                                                                               
SUK026   OC    0(8,R3),0(R3)       END OF SUM TABLE                             
         BZ    SUK030                                                           
         CLC   DUB,0(R3)                                                        
         BE    SUK031                                                           
*                                                                               
         LA    R3,32(R3)           TRY NEXT ENTRY                               
         B     SUK026                                                           
*                                                                               
SUK030   CLI   TBJCLASS,X'FF'                                                   
         BE    SUK050                                                           
*                                                                               
         MVC   0(8,R3),DUB         SET THIS ENTRY                               
         XC    32(32,R3),32(R3)    CLEAR NEXT ENTRY                             
*                                                                               
         ICM   R1,15,TABCOUNT                                                   
         LA    R1,1(R1)                                                         
         STCM  R1,15,TABCOUNT                                                   
*                                                                               
SUK031   OC    TBJETIME,TBJETIME   IF WE HAVE ETIME THEN READY                  
         BNZ   SUK032                                                           
         OC    TBJMONS,TBJMONS                                                  
         BNZ   SUK033                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,8(R3)          BUMP SUBMITTED COUNT                         
         LA    R1,1(R1)                                                         
         STCM  R1,3,8(R3)                                                       
*                                                                               
         LH    R1,TOTSUB           BUMP TOTSUB                                  
         LA    R1,1(R1)                                                         
         STH   R1,TOTSUB                                                        
*                                                                               
         L     RF,CURTIME                                                       
         SR    RF,RE               SUBTRACT SUB TIME FROM NOW TIME              
         A     RF,18(R3)                                                        
         ST    RF,18(R3)           SAVE TOTAL WAIT TIME BACK                    
         L     RF,CURTIME                                                       
         SR    RF,RE               SUBTRACT SUB TIME FROM NOW TIME              
         A     RF,AVETIME                                                       
         ST    RF,AVETIME          SAVE TOTAL WAIT TIME BACK                    
         B     SUK050                                                           
*                                                                               
SUK032   SR    R1,R1                                                            
         ICM   R1,3,12(R3)         BUMP READY COUNT                             
         LA    R1,1(R1)                                                         
         STCM  R1,3,12(R3)                                                      
*                                                                               
         TM    TBJSTAT,TBJERROR    WAS IT AN ERROR                              
         BZ    SUK050                                                           
         SR    R1,R1                                                            
         ICM   R1,3,14(R3)         BUMP ERROR COUNT                             
         LA    R1,1(R1)                                                         
         STCM  R1,3,14(R3)                                                      
         B     SUK050                                                           
*                                                                               
SUK033   SR    R1,R1                                                            
         ICM   R1,3,10(R3)         BUMP RUNNING COUNT                           
         LA    R1,1(R1)                                                         
         STCM  R1,3,10(R3)                                                      
         B     SUK050                                                           
*                                                                               
SUK040   AH    R2,JOBTABL          NEXT ENTRY                                   
         C     R2,AJOBTABX         DON'T RUN OFF TABLE END                      
         BL    SUK020                                                           
         B     SUK060                                                           
*                                                                               
SUK050   AH    R2,JOBTABL          NEXT ENTRY                                   
         BCT   R0,SUK020                                                        
SUK060   SAC   0                                                                
*                                                                               
         L     R2,TABCOUNT                                                      
         ICM   R0,15,ABIGBUF                                                    
         O     R0,=XL4'80000000'   DESCENDING SORT                              
         GOTO1 VQSORT,DMCB,(R0),(R2),32,2,8                                     
*                                                                               
         CLI   ACT.ACTACT,ACTTQUE  TEST QUEUE                                   
         BE    SUKQUEUE                                                         
*                                                                               
         GOTO1 ACALLOV,DMCB,RUNHEDH,X'D90158FD',0                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,RUNHEDH          SET HEADER FROM DUB1                         
         MVC   8(8,R4),DUB1                                                     
         MVC   52(8,R4),DUB1                                                    
*                                                                               
         L     R2,ABIGBUF                                                       
         LA    R3,RNSLIN1H         FIRST FIELD                                  
         USING SUKLINED,R3                                                      
*                                                                               
SUK070   OI    5(R3),X'80'                                                      
         LA    R3,8(R3)                                                         
         OC    0(2,R2),0(R2)                                                    
         BZ    SUKX                                                             
*                                                                               
SUK080   MVC   SUKLCAT,0(R2)                                                    
         CLI   SUKCHR,C'U'                                                      
         BNE   SUK081                                                           
         MVC   GIUSER,0(R2)                                                     
         BAS   RE,GETUSER                                                       
         MVC   SUKLCAT,GIUSERID                                                 
*                                                                               
SUK081   CLI   SUKCHR,C'A'                                                      
         BNE   SUK082                                                           
         MVC   GTADV,0(R2)         SET ADV SOURCE                               
         BAS   RE,GETADV                                                        
         MVC   SUKLCAT(4),GTADVX                                                
*                                                                               
SUK082   EQU   *                                                                
*                                                                               
SUK089   EDIT  (B2,08(R2)),(4,SUKLSUB),ZERO=NOBLANK                             
         EDIT  (B2,10(R2)),(3,SUKLRNG),ZERO=NOBLANK                             
         EDIT  (B2,12(R2)),(3,SUKLRDY),ZERO=NOBLANK                             
         EDIT  (B2,14(R2)),(3,SUKLERR),ZERO=NOBLANK                             
         EDIT  (B2,16(R2)),(3,SUKLUNK),ZERO=NOBLANK                             
         CLC   18(2,R2),=X'FFFF'                                                
         BE    SUK75                                                            
         ICM   R1,15,18(R2)                                                     
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         LTR   R1,R1                                                            
         BNZ   SUK76                                                            
SUK75    MVC   SUKLHH(4),=C'NONE'                                               
         B     SUK77                                                            
*                                                                               
SUK76    XC    FULL,FULL                                                        
         MVC   FULL+2(2),8(R2)     NUMBER OF SUBMITTED                          
         SR    R0,R0                                                            
         D     R0,FULL                                                          
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUKLMM,DUB                                                       
         MVI   SUKLCOL,C':'                                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUKLHH,DUB                                                       
*                                                                               
SUK77    AH    R3,=Y(SUKLLEN)      NEXT FIELD HEADER                            
         LA    R2,32(R2)                                                        
         LA    R1,RNSMOREH         END OF LIST                                  
         CR    R3,R1                                                            
         BL    SUK070                                                           
*                                                                               
         MVI   SCRFULL,YES                                                      
         B     SUKX                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
************************************************************                    
* FORMAT MESSAGE                                           *                    
************************************************************                    
SUKQUEUE DS    0H                                                               
         MVC   RUNHED,SPACES                                                    
         MVC   RUNHEDU,SPACES                                                   
         MVC   RUNHED,QUEUEL1                                                   
         MVC   RUNHEDU,QUEUEL2                                                  
         MVC   RUNHED(8),DUB1                                                   
         LA    R3,RUNLIN                                                        
         L     R2,ABIGBUF                                                       
*                                                                               
SUMQ010  ST    R3,FULL                                                          
         MVC   0(8,R3),0(R2)                                                    
*                                                                               
         CLI   SUKCHR,C'U'                                                      
         BNE   SUMQ020                                                          
         MVC   GIUSER,0(R2)        SET USERID NUM                               
         BAS   RE,GETUSER                                                       
         MVC   0(8,R3),GIUSERID                                                 
*                                                                               
SUMQ020  CLI   SUKCHR,C'A'                                                      
         BNE   SUMQ030                                                          
         MVC   GTADV,0(R2)         SET ADV SOURCE                               
         BAS   RE,GETADV                                                        
         MVC   0(4,R3),GTADVX                                                   
*                                                                               
SUMQ030  LA    R3,9(R3)                                                         
*                                                                               
         SR    R1,R1               SET R1 TO JOBS/4                             
         ICM   R1,3,8(R2)                                                       
         SRL   R1,2                                                             
         CHI   R1,70               280 MAX                                      
         BL    *+8                                                              
         LA    R1,70                                                            
*                                                                               
         LTR   R1,R1                                                            
         BZ    SUMQ035                                                          
         BCTR  R1,0                BUILDS A LINE OF ****S                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),STARS                                                    
         AR    R3,R1                                                            
*                                                                               
SUMQ035  SR    R1,R1               SET R1 TO RUNNING                            
         ICM   R1,3,10(R2)                                                      
         BZ    *+20                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=C'<<<<<<<<'                                             
         B     *+8                                                              
         MVI   0(R3),C'|'                                                       
         LA    R1,1(R1)                                                         
         AR    R3,R1                                                            
*                                                                               
         SR    R1,R1               SET R1 TO READY                              
         ICM   R1,3,12(R2)                                                      
         SRL   R1,2                                                             
         LTR   R1,R1                                                            
         BZ    SUMQ040                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DASHES                                                   
*                                                                               
SUMQ040  LA    R2,32(R2)           NEXT CLASS                                   
         OC    0(2,R2),0(R2)                                                    
         BZ    SUKX                                                             
*                                                                               
         L     R3,FULL                                                          
         LLC   R0,0(R3)            FIELD LENGTH                                 
         AR    R3,R0               NEXT SCREEN LINE                             
         LA    R1,RUNXXXH                                                       
         CR    R3,R1                                                            
         BL    SUMQ010                                                          
         B     SUKX                                                             
         EJECT                                                                  
************************************************************                    
* FORMAT MESSAGE                                           *                    
************************************************************                    
SUKX     SAC   0                                                                
         CLI   YESMORE,YES         INDICATE THERE WERE MORE                     
         BNE   SUMXA                                                            
         MVC   RNSMORE,=C'-->'                                                  
         OI    RNSMOREH+5,X'80'                                                 
*                                                                               
SUMXA    LA    R1,RUNACTH          SET FADR FOR ERROR/EXIT                      
         ST    R1,FADR                                                          
         XC    FLD(L'RUNMSG),FLD   FORMAT MESSAGE IN FLD FOR EXIT               
         MVC   FLD(L'SUMMSG),SUMMSG                                             
         LA    R7,FLD                                                           
         LA    R7,L'SUMMSG-4(R7)                                                
         EDIT  (B2,TOTSUB),(4,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                    
         AR    R7,R0                                                            
         MVC   0(L'SUMMSG1,R7),SUMMSG1                                          
         LA    R7,L'SUMMSG1-5(R7)                                               
         OC    AVETIME,AVETIME                                                  
         BZ    SUM100                                                           
         L     R1,AVETIME                                                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),TOTSUB                                                 
         SR    R0,R0                                                            
         D     R0,FULL                                                          
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R7),DUB                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R7),DUB                                                      
SUM100   DS    0H                                                               
         LA    R7,5(R7)                                                         
         OC    TOTSUBB,TOTSUBB     ANY TYPE B                                   
         BZ    SUK900                                                           
         MVC   0(L'SUMMSG2,R7),SUMMSG2                                          
         LA    R7,L'SUMMSG2-4(R7)                                               
         EDIT  (B2,TOTSUBB),(4,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                   
         AR    R7,R0                                                            
         MVC   0(L'SUMMSG3,R7),SUMMSG3                                          
         LA    R7,L'SUMMSG3-5(R7)                                               
         DS    0H                  R1 = TIME IN SECONDS ONLY                    
         OC    AVETIMEB,AVETIMEB                                                
         BZ    SUK900                                                           
         L     R1,AVETIMEB                                                      
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),TOTSUB                                                 
         SR    R0,R0                                                            
         D     R0,FULL                                                          
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R7),DUB                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R7),DUB                                                      
SUK900   MVC   RUNMSG,FLD                                                       
         L     RD,SAVERD                                                        
         B     XMODX                                                            
         EJECT                                                                  
*************************************************************                   
*        GET USERID FROM 2 CHR ID NUMBER                    *                   
*************************************************************                   
GETUSER  NTR1                                                                   
         CLC   GIUSER,GIPREV                                                    
         BE    GETUSRX                                                          
         MVC   GIPREV,GIUSER                                                    
         LA    R7,IO               BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),GIUSER                                               
         NI    CTIKID+8,X'7F'      UNSET GENERIC FLAG                           
         GOTO1 ADMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                               
         CLI   8(R1),0                                                          
         BNE   GETUSR12            NNNNNN IF REC NOT FOUND                      
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
GETUSR10 AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST ID ELEMENT                              
         BNE   *+14                                                             
         MVC   GIUSERID,2(R7)      GET ID NAME                                  
         B     GETUSR20                                                         
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         BNE   GETUSR10            BUT DROP THROUGH IF LAST                     
*                                                                               
GETUSR12 EDIT  (B2,GIUSER),(6,GIUSERID),FILL=0,DUB=SDUB,WRK=SWORK1              
*                                                                               
GETUSR20 LA    RF,0                                                             
         LA    RE,GIUSERID                                                      
GETUSR21 CLI   0(RE),X'40'                                                      
         BE    GETUSR30                                                         
         CLI   0(RE),0                                                          
         BE    GETUSR30                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CH    RF,=H'8'                                                         
         BL    GETUSR21                                                         
GETUSR30 STC   RF,GIULEN                                                        
GETUSRX  XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
************************************************************                    
*        GET ADV NAME FROM NUMBER                          *                    
************************************************************                    
GETADV   NTR1                                                                   
         L     RE,AFID             RE=FACIDTAB                                  
GETAD010 MVC   BYTE,GTADV          COPY NUMBER TO BYTE                          
         NI    BYTE,X'0F'          REMOVE AOR BITS FROM BYTE                    
         CLC   BYTE,4(RE)          TEST ADV NUMBER                              
         BE    GETAD020                                                         
         CLI   4(RE),8             IF WE ARE ON SYS8 THEN NOT FOUND             
         BE    GETADVX                                                          
         LA    RE,8(RE)                                                         
         B     GETAD010                                                         
*                                                                               
GETAD020 MVC   GTADVN,0(RE)        SAVE 4CHR NAME                               
         MVC   GTADVX,0(RE)                                                     
         CLI   GTADVX+3,C' '       IF 4 CHR ADVNAME MOVE C4 TO C3               
         BE    *+10                                                             
         MVC   GTADVX+2(1),GTADVX+3                                             
         MVI   GTADVX+3,C' '                                                    
         SR    R1,R1                                                            
         IC    R1,GTADV                                                         
         SRL   R1,4                SET R1 TO AOR NUM                            
*                                                                               
         LTR   R1,R1               IS THIS AN AOR                               
         BZ    *+12                                                             
         LA    R1,X'C0'(R1)        CONVERT TO ABC                               
         STC   R1,GTADVX+3         AND SAVE IN LAST CHR                         
*                                                                               
GETADVX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO GET USER NAME FROM ID NUMBER                             *         
* NTRY: HALF   = USER ID                                              *         
* EXIT: WORK2  = USER NAME                                            *         
***********************************************************************         
GETUNAM  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK2,SPACES                                                     
*                                                                               
         LA    R1,KEY              EDIT USER-ID                                 
         USING CTIKEY,R1                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID+8(2),HALF                                                 
         GOTO1 ADMGR,DMCB,DMREAD,CTFILE,KEY,IO                                  
         CLI   8(R1),0             TEST FOR ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,IO+(CTIDATA-CTIREC)                                           
         USING CTDSCD,R1                                                        
         XR    RF,RF                                                            
UFM06    CLI   CTDSCEL,0           LOCATE ALPHA USER-ID ELEMENT                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R1,RF,UFM06                                                      
*                                                                               
         IC    RF,CTDSCLEN                                                      
         AHI   RF,-(CTDSC-CTDSCD+1)                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),CTDSC                                                   
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* TRANSFER CONTROL                                                    *         
***********************************************************************         
GOPQ     L     R1,SRPAUTL          LOCATE UTL                                   
         L     R1,TBUFF-UTLD(R1)   LOCATE TERMINAL BUFFER                       
         MVI   0(R1),X'0B'         LENGTH FOR =PQ,JOB                           
         MVC   1(2,R1),RUNSRVH+2   INSERT SCREEN ADDRESSES                      
         MVC   3(7,R1),=C'=PQ,JOB' =PQ,JOB IN SRVID FIELD                       
         MVI   10(R1),0            END MARKER                                   
         MVC   RUNSRV(8),=C'=GOBACK '                                           
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION                                                     *         
***********************************************************************         
VALP1    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RUNACTH                                                       
         USING FHD,R2                                                           
         ST    R2,AHELP                                                         
         L     R3,AACTTAB                                                       
         USING ACTTABD,R3                                                       
*                                                                               
VP102    CLI   FHIL,0                                                           
         BNE   VP104                                                            
         CLI   DDS,YES             DEFAULT FOR DDS SUMMARY OR DISP              
         BNE   *+8                                                              
         L     R3,AACTDEF                                                       
*                                                                               
         LH    RF,ACTUNAM                                                       
         AR    RF,RC                                                            
         MVC   FHDA(8),0(RF)                                                    
         MVI   FHIL,3                                                           
*                                                                               
VP104    CLI   FHDA,C'?'           INPUT MUST BE AT LEAST 2 CHARACTERS          
         BE    HELPOUT                                                          
         BNE   VP106                                                            
         BRAS  RE,P1HELP                                                        
         BH    VP102               SELECT FROM HELP BY PFKEY                    
         MVI   INHELP,YES                                                       
         B     EXITL                                                            
*                                                                               
VP106    XR    RF,RF                                                            
         ICM   RF,1,FHIL           GET INPUT LENGTH                             
         BCTR  RF,0                                                             
         CHI   RF,1                MORE THAN 2 CHARACTERS INPUT?                
*        BNH   *+8                 NO                                           
*        LHI   RF,1                ONLY USE FIRST 2 CHARS TO VALIDATE           
*                                                                               
VP108    CLI   ACTUNAM,255         EOT?                                         
         BE    EIAC                YES - INVALID ACTION                         
*                                                                               
         CLI   DDS,YES             DDS CAN DO ANYTHING                          
         BE    VP110                                                            
         CLI   ACTUSER,ACTUALL     OTHERWISE ONLY 'ALL' ACTIONS                 
         BNE   VP112                                                            
*                                                                               
VP110    LH    R1,ACTUNAM                                                       
         AR    R1,RC                                                            
         EX    RF,VP1CLC                                                        
         BE    VP114                                                            
         LH    R1,ACTLNAM                                                       
         AR    R1,RC                                                            
         EX    RF,VP1CLC                                                        
         BE    VP114                                                            
*                                                                               
VP112    AHI   R3,ACTTABL                                                       
         B     VP108                                                            
*                                                                               
VP114    MVC   ACTNTRY,ACTTABD                                                  
         LH    R1,ACTLNAM                                                       
         AR    R1,RC                                                            
         MVC   FHDA(8),0(R1)                                                    
         B     EXITOK                                                           
*                                                                               
VP1CLC   CLC   FHDA(0),0(R1)                                                    
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS                                                    *         
***********************************************************************         
VALP2    NTR1  BASE=*,LABEL=*                                                   
         XC    OPTI,OPTI           CLEAR INPUT OPTIONS BITS                     
         LA    R2,RUNOPTH          OPTIONS FIELD                                
         ST    R2,AHELP                                                         
         USING FHD,R2                                                           
         CLI   FHDA,C'?'                                                        
         BE    HELPOUT                                                          
*                                                                               
         OI    FHOI,FHOICU         SET CURSOR                                   
*                                                                               
         CLI   FHIL,0              ANY INPUT?                                   
         BNE   VP202               YES                                          
         CLI   ACT.ACTACT,ACTTCLR  TEST CLEAR                                   
         BE    EROM                                                             
         CLI   ACT.ACTACT,ACTTPRI  TEST PRIORITY                                
         BE    EROM                                                             
         CLI   ACT.ACTACT,ACTTPUR  TEST PURGE                                   
         BE    EROM                                                             
         B     EXITOK                                                           
*                                                                               
VP202    GOTO1 VSCANNER,DMCB,RUNOPTH,(6,SCANBLK),C',=,='                        
         XR    R0,R0                                                            
         IC    R0,4(R1)                                                         
*                                                                               
         LA    R2,SCANBLK                                                       
         USING SCANBLKD,R2                                                      
         CLI   ACT.ACTACT,ACTTPRI  PRIORITY VALIDATED DIFFERENTLY               
         BNE   *+12                                                             
         BRAS  RE,VALPRI                                                        
         B     EXITOK                                                           
*                                                                               
         CLI   ACT.ACTACT,ACTTPUR  PURGE VALIDATED AS PRIORITY                  
         BNE   *+12                                                             
         BRAS  RE,VALPUR                                                        
         B     EXITOK                                                           
*                                                                               
VP204    CLI   SC1STLEN,1          OPTION LEN MUST BE BETWEEN 1 AND 8           
         BL    EIIF                                                             
         CLI   SC1STLEN,8                                                       
         BH    EIIF                                                             
*                                                                               
         L     R3,AOPTTAB          R3=A(OPTIONS TABLE)                          
VP206    CLI   0(R3),0             TEST E-O-T                                   
         BE    EIKW                                                             
*                                                                               
         XR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
*                                                                               
         EX    0,0(R3)             SET RF TO A(TEXT)                            
         EX    R1,*+8              COMPARE KEYWORD                              
         BE    VP210                                                            
         CLC   0(0,RF),SC1STFLD                                                 
*                                                                               
VP208    AHI   R3,L'OPTTAB         BUMP TO NEXT OPTION TABLE ENTRY              
         B     VP206                                                            
*                                                                               
VP210    L     RF,4(R3)            PICK UP A(VAL ROUTINE)                       
         A     RF,RELO                                                          
         BASR  RE,RF               GOTO VALIDATION ROUTINE                      
*                                                                               
VP212    AHI   R2,SCBLKLQ                                                       
         BCT   R0,VP204                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FIELD FOR PRIORITY CHANGE                            *         
* FORMAT IS: AUTH,PRI#,USER,REP/#####                                 *         
* NTRY: R0     = COUNT OF SCAN FIELDS                                 *         
*       R2     = A(SCANBLK)                                           *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALPRI   NTR1  BASE=*,LABEL=*                                                   
         CHI   R0,4                                                             
         BNE   EROM                                                             
         XC    DUB,DUB                                                          
         LA    RF,PRIAUTH                                                       
*                                                                               
VPRI02   CLI   0(RF),X'FF'                                                      
         BE    EIAU                                                             
         CLC   0(L'PRIAUTH,RF),SC1STFLD                                         
         BE    *+12                                                             
         AHI   RF,L'PRIAUTH                                                     
         B     VPRI02                                                           
*                                                                               
         MVC   AUTH,0(RF)                                                       
         AHI   R2,SCBLKLQ                                                       
*                                                                               
         TM    SC1STVAL,SCNUMQ                                                  
         BZ    EKEY                                                             
         ICM   R0,15,SC1STNUM                                                   
         CHI   R0,10                                                            
         BNL   EKEY                                                             
*                                                                               
         MVC   JOBPRI,SC1STFLD     PRIORITY 1 - 9                               
*                                                                               
         AHI   R2,SCBLKLQ                                                       
*                                                                               
         MVC   SC2NDLEN,SC1STLEN   FIX FOR VALUSER                              
         MVC   SC2NDFLD,SC1STFLD                                                
         BRAS  RE,VALUSER          SETS JOBUSER - NO RETURN ON ERROR            
*                                                                               
         AHI   R2,SCBLKLQ                                                       
*                                                                               
         MVC   SC2NDLEN,SC1STLEN   FIX FOR VALSUBID                             
         MVC   SC2NDFLD,SC1STFLD                                                
         BRAS  RE,VALSUBID         SETS JOBUSER - NO RETURN ON ERROR            
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
PRIAUTH  DS    0CL8                                                             
         DC    CL8'AHYDN   '                                                    
         DC    CL8'GCORL   '                                                    
         DC    CL8'TCLEL   '                                                    
         DC    CL8'RMORL   '                                                    
         DC    CL8'AWILN   '                                                    
         DC    CL8'EJORN   '                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FIELD FOR PRIORITY CHANGE                            *         
* FORMAT IS: AUTH,USER,REP/#####                                      *         
* NTRY: R0     = COUNT OF SCAN FIELDS                                 *         
*       R2     = A(SCANBLK)                                           *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALPUR   NTR1  BASE=*,LABEL=*                                                   
         CHI   R0,3                                                             
         BNE   EROM                                                             
         XC    DUB,DUB                                                          
         LA    RF,PURAUTH                                                       
*                                                                               
VPUR02   CLI   0(RF),X'FF'                                                      
         BE    EIAU                                                             
         CLC   0(L'PURAUTH,RF),SC1STFLD                                         
         BE    *+12                                                             
         AHI   RF,L'PURAUTH                                                     
         B     VPUR02                                                           
*                                                                               
         MVC   AUTH,0(RF)                                                       
         AHI   R2,SCBLKLQ                                                       
*                                                                               
         MVC   SC2NDLEN,SC1STLEN   FIX FOR VALUSER                              
         MVC   SC2NDFLD,SC1STFLD                                                
         BRAS  RE,VALUSER          SETS JOBUSER - NO RETURN ON ERROR            
*                                                                               
         AHI   R2,SCBLKLQ                                                       
*                                                                               
         MVC   SC2NDLEN,SC1STLEN   FIX FOR VALSUBID                             
         MVC   SC2NDFLD,SC1STFLD                                                
         BRAS  RE,VALSUBID         SETS JOBUSER - NO RETURN ON ERROR            
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
PURAUTH  DS    0CL8                                                             
         DC    CL8'AHYDN   '                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TERMINAL NUMBER                                 *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALTRM   NTR1  BASE=*,LABEL=*                                                   
         MVI   SUKCHR,C'T'                                                      
         CLI   SC2NDLEN,0          TERMINAL INPUT?                              
         BNE   VTRM02              NO                                           
         MVC   TERMLUID,MYLUID     USE MY TERMINAL                              
         XC    JOBUSER,JOBUSER     AND ANY USERID I HAVE                        
         B     EXITOK                                                           
*                                                                               
VTRM02   CLI   DDS,YES             ONLY DDS CAN SEE A TERMINAL                  
         BNE   EIIF                                                             
         MVC   TERMLUID,SC2NDFLD                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE JOBTYPE                                         *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALTYP   MVC   JOBTYPE,SC2NDFLD                                                 
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SYSTEM                                          *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALSYS   MVC   JOBSYS,SC2NDFLD                                                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SORT BY OLDEST TIME                             *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALTIME  MVI   USETIME,YES                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SORT BY NUMBER RUNNING                          *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALRUN   MVI   USERUN,YES                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SHOW REGULAR ONLY                               *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALUPD   MVI   UPDS,YES                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SHOW REGULAR ONLY                               *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALREG   DS    0H                                                               
*&&UK*&& MVI   LONGS,C'R'                                                       
*&&US*&& MVI   LONGS,JCMED                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SHOW LONG ONLY                                  *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALLONG  MVI   LONGS,JCLONG                                                     
         BR    RE                                                               
                                                                                
VALCOMS  MVI   LONGS,JCCOMSC                                                    
         BR    RE                                                               
                                                                                
VALLONGW MVI   SUKCHR,C'L'         Long wait                                    
         BR    RE                                                               
                                                                                
VALJOB   MVI   SUKCHR,C'J'         Job                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE STATUS                                          *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALSTAT  NTR1  BASE=*,LABEL=*                                                   
         XR    RF,RF                                                            
         IC    RF,SC2NDLEN                                                      
         BCTR  RF,0                                                             
*                                                                               
         L     R1,ASTATAB          SCAN STATUS TABLE                            
VSTA02   CLI   0(R1),0                                                          
         BE    EIDV                                                             
         EX    0,0(R1)             SET RF TO TEXT                               
         CLC   0(3,RF),SC2NDFLD                                                 
         BE    VSTA04                                                           
         AHI   R1,6                                                             
         B     VSTA02                                                           
*                                                                               
VSTA04   MVC   JOBFILT,4(R1)                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE CLASS                                           *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALCLS   MVC   JOBCLS,SC2NDFLD                                                  
*                                                                               
         CLI   SUKCHR,0            SET SUKCHR IF NOT SET                        
         BNER  RE                                                               
         MVI   SUKCHR,C'C'                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AGENCY                                          *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALAGY   CLI   SC2NDLEN,2          MAKE SURE RIGHT INPUT THERE                  
         BNE   EIIF                                                             
         MVC   JOBAGY,SC2NDFLD                                                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE USERID FILTER                                   *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALUSER  NTR1  BASE=*,LABEL=*                                                   
         MVI   SUKCHR,C'U'                                                      
         MVC   JOBUSERC,SC2NDFLD                                                
         CLI   SC2NDLEN,0          IS USERID BLANK                              
         BNE   *+14                                                             
         MVC   JOBUSER,MYUSER      USE MY USERID                                
         B     EXIT                                                             
*                                                                               
         CLI   DDS,YES             ONLY DDS CAN SEE ANOTHER USER                
         BNE   EIIF                                                             
*                                                                               
         LA    R3,KEY              BUILD ID RECORD KEY                          
         USING CTIKEY,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SPACES                                                    
         XR    R1,R1                                                            
         IC    R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTIKID(0),SC2NDFLD                                               
         GOTO1 ADMGR,DMCB,DMREAD,CTFILE,KEY,IO                                  
         CLI   8(R1),0                                                          
         BNE   EIID                ID RECORD NOT FOUND                          
*                                                                               
         LA    R3,IO                                                            
         LA    R3,CTIDATA                                                       
         USING CTDSCD,R3                                                        
         XR    RF,RF                                                            
VUSER02  CLI   CTDSCEL,0           LOCATE HEX USER-ID ELEMENT                   
         BE    VUSERBAD                                                         
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R3,RF,VUSER02                                                    
*                                                                               
         MVC   JOBUSER,CTDSC       SAVE USERID                                  
         B     EXIT                                                             
*                                                                               
VUSERBAD DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SUB-ID FILTER                                   *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALSUBID NTR1  BASE=*,LABEL=*                                                   
         MVC   JOBSUBID,SC2NDFLD                                                
         CLI   SC2NDLEN,3          ONLY SUBID?                                  
         BE    EXIT                                                             
         CLI   SC2NDFLD+3,C'/'     ANY REPORT NUMBER?                           
         BNE   EIDV                ERROR                                        
*                                                                               
         XR    R1,R1                                                            
         IC    R1,SC2NDLEN                                                      
         AHI   R1,-4                                                            
         BNP   EIDV                MUST BE AT LEAST 1 DIGITS                    
*                                                                               
         LA    RE,SC2NDFLD+3                                                    
         LR    RF,R1                                                            
VSID02   LA    RE,SC2NDFLD+3(RF)                                                
         CLI   0(RE),C'0'                                                       
         BL    EIDV                                                             
         CLI   0(RE),C'9'                                                       
         BH    EIDV                                                             
         BCT   RF,VSID02                                                        
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SC2NDFLD+4(0)                                                
         CVB   R1,DUB                                                           
         STCM  R1,3,JOBREPNO                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ADV FILTER                                      *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
VALADV   NTR1  BASE=*,LABEL=*                                                   
         L     R1,AFID                                                          
*                                                                               
         MVI   SUKCHR,C'A'         SET SUKCHR                                   
         CLI   SC2NDFLD,C' '       EXIT IF THATS ALL                            
         BNH   EXITOK                                                           
*                                                                               
VADV02   CLI   0(R1),X'FF'         TEST EOT                                     
         BE    EIDV                                                             
*                                                                               
         CLC   0(4,R1),SC2NDFLD    MATCH ADV                                    
         BE    *+12                                                             
         LA    R1,8(R1)                                                         
         B     VADV02                                                           
*                                                                               
         MVC   JOBADV,4(R1)                                                     
         NI    JOBADV,X'0F'                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE MONSOON                                         *         
***********************************************************************         
VALMON   MVC   JOBMONS,SC2NDFLD                                                 
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE JCL FILTER                                      *         
***********************************************************************         
VALJCL   MVC   JCLFILT,SC2NDFLD                                                 
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOCK JOB AND CLASS TABLE IN CORRECT ORDER                           *         
***********************************************************************         
LOCKBOTH NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,LOCKCLS                                                       
         BRAS  RE,LOCKJOB                                                       
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE JOB AND CLASS TABLE IN CORRECT ORDER                           *         
***********************************************************************         
FREEBOTH NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEJOB                                                       
         BRAS  RE,FREECLS                                                       
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ENQUIRE ON JOB TABLE (GET HEADER)                                   *         
***********************************************************************         
ENQJOB   NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTJOB)                                              
         MVI   DMCB,X'20'                                                       
         GOTO1 ALOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         MVC   DSPHD,0(RF)                                                      
         NI    JT.DSPTFRST,X'3F'                                                
         ICM   RF,15,JT.DSPTEND                                                 
         AHI   RF,-2048                                                         
         STCM  RF,15,JT.DSPTEND                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOCK JOB TABLE                                                      *         
***********************************************************************         
LOCKJOB  NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTJOB)                                              
         GOTO1 ALOCKSPC,DMCB                                                    
         MVI   JOBLOCK,YES                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE JOB TABLE                                                      *         
***********************************************************************         
FREEJOB  NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTJOB)                                              
         OI    DMCB,X'10'                                                       
         GOTO1 ALOCKSPC,DMCB                                                    
         MVI   JOBLOCK,NO                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ENQUIRE ON CLASS TABLE (GET HEADER)                                 *         
***********************************************************************         
ENQCLS   NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         MVI   DMCB,X'20'                                                       
         GOTO1 ALOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         MVC   CSPHD,0(RF)                                                      
         NI    CL.DSPTFRST,X'3F'                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOCK CLASS TABLE                                                    *         
***********************************************************************         
LOCKCLS  NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         GOTO1 ALOCKSPC,DMCB                                                    
         MVI   CLSLOCK,YES                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE CLASS TABLE                                                    *         
***********************************************************************         
FREECLS  NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         OI    DMCB,X'10'                                                       
         GOTO1 ALOCKSPC,DMCB                                                    
         MVI   CLSLOCK,NO                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ S/R PAGE 11                                                    *         
***********************************************************************         
RDTWAB   NTR1  BASE=*,LABEL=*                                                   
         MVI   TWAREAD,YES                                                      
*                                                                               
         L     R1,SRPAUTL                                                       
         LA    R0,SRPAGENO         READ & LOCK SAVE PAGE                        
         SLL   R0,32-8                                                          
         ICM   R0,3,TNUM-UTLD(R1)                                               
         ST    R0,TWAPAGE                                                       
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADMGR,DMCB,(X'80',DMREAD),TEMPSTR,TWAPAGE,SRPATIA                
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE TO CONSOLE                                                    *         
***********************************************************************         
WTO      NTR1  BASE=*,LABEL=*                                                   
         MVC   OPERMSG(OPERMSGL),SPACES                                         
         MVC   OPERID,=CL10'+PRIORITY+'                                         
         MVC   OPERNAME,AUTH                                                    
         MVC   OPERT1,=CL04'SET='                                               
         MVC   OPERPRI,JOBPRI                                                   
         MVC   OPERT2,=CL02'U='                                                 
         MVC   OPERUSER,JOBUSERC                                                
         LA    R2,OPERUSER+L'OPERUSER                                           
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','                                                       
         MVC   2(3,R2),JOBSUBID                                                 
         AHI   R2,5                                                             
         MVI   0(R2),C'/'                                                       
         MVC   1(3,R2),=CL3'ALL'                                                
*                                                                               
         OC    JOBREPNO,JOBREPNO                                                
         BZ    WTO02                                                            
         MVC   1(3,R2),SPACES                                                   
         EDIT  (B2,JOBREPNO),(5,1(R2)),ALIGN=LEFT                               
*                                                                               
WTO02    GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPERMSG,OPERMSGL,C'LVL1'                   
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TO CONSOLE                                                    *         
***********************************************************************         
WTO1     NTR1  BASE=*,LABEL=*                                                   
         MVC   OPERMSG(OPERMSGL),SPACES                                         
         MVC   OPERID,=CL10'+PURGE   +'                                         
         MVC   OPERNAME,AUTH                                                    
         MVC   OPERT1,=CL04'SET='                                               
         MVC   OPERPRI,JOBPRI                                                   
         MVC   OPERT2,=CL02'U='                                                 
         MVC   OPERUSER,JOBUSERC                                                
         LA    R2,OPERUSER+L'OPERUSER                                           
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','                                                       
         MVC   2(3,R2),JOBSUBID                                                 
         AHI   R2,5                                                             
         MVI   0(R2),C'/'                                                       
         MVC   1(3,R2),=CL3'ALL'                                                
*                                                                               
         OC    JOBREPNO,JOBREPNO                                                
         BZ    WTO102                                                           
         MVC   1(3,R2),SPACES                                                   
         EDIT  (B2,JOBREPNO),(5,1(R2)),ALIGN=LEFT                               
*                                                                               
WTO102   GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPERMSG,OPERMSGL,C'LVL1'                   
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TO CONSOLE FOR 'CLEAR' ACTION                                 *         
***********************************************************************         
WTOC     NTR1  BASE=*,LABEL=*                                                   
         MVC   OPERMSG(OPERMSGL),SPACES                                         
         MVC   OPERID,=CL10'+CLEAR   +'                                         
*                                                                               
         MVC   OPERCLR,SWORK1                                                   
*                                                                               
         GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPERMSG,OPERMSGL2,C'LVL1'                  
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TO CONSOLE FOR 'PURGE' ACTION                                 *         
***********************************************************************         
WTOP     NTR1  BASE=*,LABEL=*                                                   
         MVC   OPERMSG(OPERMSGL),SPACES                                         
         MVC   OPERID,=CL10'+PURGE   +'                                         
*                                                                               
         MVC   OPERCLR,SWORK1                                                   
*                                                                               
         GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPERMSG,OPERMSGL2,C'LVL1'                  
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY HEADLINES FOR DDS TERMINAL                                  *         
***********************************************************************         
DFHDR    NTR1  BASE=*,LABEL=*                                                   
         MVC   RUNHED,SPACES                                                    
         MVC   RUNHEDU,SPACES                                                   
X        USING RUNLLIN,RUNHED                                                   
Y        USING RUNLLIN,RUNHEDU                                                  
         MVC   X.RUNLTIME,=CL8'Submit  '                                        
         MVC   Y.RUNLTIME,=CL8'Time    '                                        
         MVC   X.RUNLCLS(4),=CL4'Cls-'                                          
         MVC   Y.RUNLCLS(4),=CL4'-Agy'                                          
         MVC   X.RUNLADV(4),=CL4'Fac '                                          
         MVC   Y.RUNLADV(4),=CL4'--- '                                          
         MVC   X.RUNLPRTY,=CL1'P'                                               
         MVC   Y.RUNLPRTY,=CL1'r'                                               
         MVC   X.RUNLNAME,=CL8'Job Name'                                        
         MVC   Y.RUNLNAME,=CL8'--------'                                        
         MVC   X.RUNLPQNO,=CL1'P'                                               
         MVC   Y.RUNLPQNO,=CL1'Q'                                               
         MVC   X.RUNLMONS,=CL7'MONS#/ '                                         
         MVC   Y.RUNLMONS,=CL7'ASID   '                                         
         MVC   X.RUNLTSYM,=CL8'Submit  '                                        
         MVC   Y.RUNLTSYM,=CL8'Terminal'                                        
         MVC   X.RUNLUSER,=CL8'User Id '                                        
         MVC   Y.RUNLUSER,=CL8'------- '                                        
         MVC   X.RUNLREPT(9),=CL9'Report Id'                                    
         MVC   Y.RUNLREPT(9),=CL9'---------'                                    
         MVC   X.RUNLSTAT,=CL9'Sta'                                             
         MVC   Y.RUNLSTAT,=CL9'tus'                                             
         MVC   X.RUNLTYP,=CL2'Ty'                                               
         MVC   Y.RUNLTYP,=CL2'--'                                               
         B     EXITOK                                                           
         DROP  X,Y                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY HEADLINES FOR USER TERMINAL                                 *         
***********************************************************************         
UFHDR    NTR1  BASE=*,LABEL=*                                                   
         MVC   RUNHED,SPACES                                                    
         MVC   RUNHEDU,SPACES                                                   
X        USING USRLLIN,RUNHED                                                   
Y        USING USRLLIN,RUNHEDU                                                  
         MVC   X.USRLTIME,=CL8'Submit  '                                        
         MVC   Y.USRLTIME,=CL8'Time    '                                        
         MVC   X.USRLUSER,=CL8'User Id '                                        
         MVC   Y.USRLUSER,=CL8'---- -- '                                        
         MVC   X.USRLREPT(9),=CL9'Report Id'                                    
         MVC   Y.USRLREPT(9),=CL9'---------'                                    
         MVC   X.USRLTYPE,=CL10'Report'                                         
         MVC   Y.USRLTYPE,=CL10'Type       '                                    
         MVC   X.USRLSTAT,=CL9'Status   '                                       
         MVC   Y.USRLSTAT,=CL9'------   '                                       
         B     EXITOK                                                           
         DROP  X,Y                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO BUILD SUMMARY SCREEN HEADINGS                            *         
***********************************************************************         
DSHDR    NTR1  BASE=*,LABEL=*                                                   
         MVC   RUNHED,SPACES                                                    
         MVC   RUNHEDU,SPACES                                                   
X        USING SUMLLIN,RUNHED                                                   
Y        USING SUMLLIN,RUNHEDU                                                  
         MVC   X.SUMLCLS(3),=CL3'Cla'                                           
         MVC   Y.SUMLCLS,=CL2'ss'                                               
         MVC   X.SUMLSUB(21),=CL21'=Submit details======'                       
         MVC   Y.SUMLSUB,=CL3'Now'                                              
         MVC   Y.SUMTSUB,=CL5'Today'                                            
         MVC   Y.SUMWAIT,=CL5'Av/Wt'                                            
         MVC   Y.SUMSUBT,=CL5'First'                                            
         MVC   X.SUMLRUN(21),=CL21'=Running details====='                       
         MVC   Y.SUMLRUN,=CL3'Now'                                              
         MVC   Y.SUMTRUN,=CL5'Today'                                            
         MVC   Y.SUMRUNW,=CL5'First'                                            
         MVC   Y.SUMRUNT,=CL5'Av/Rt'                                            
         MVC   X.SUMLRDY(9),=CL9'=Ready==='                                     
         MVC   Y.SUMLRDY,=CL3'   '                                              
         MVC   Y.SUMTRDY,=CL5'Today'                                            
         B     EXITOK                                                           
         DROP  X,Y                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DISPLAY TIME INTO WORK2(8)                                          *         
* NTRY: RF     = TIME IN 1/100 OF A SECOND UNITS                      *         
* EXIT: WORK2  = TIME HH:MM:SS                                        *         
***********************************************************************         
SHOWTIME NTR1  BASE=*,LABEL=*                                                   
         XR    RE,RE                                                            
         MVC   WORK2(8),=C'00:00:00'                                            
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK2),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK2+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK2+6),FILL=0    SECS                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY WAIT TIME INTO WORK2(5)                                     *         
* NTRY: RF     = TIME IN 1/100 OF A SECOND UNITS                      *         
* EXIT: WORK2  = TIME 0H 00 - TIME IN HOURS AND MINUTES (IF >1H)      *         
*                     00:00 - TIME IN MINUTES AND SECONDS (IF <1H)    *         
***********************************************************************         
SHOWWAIT NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK2(5),=CL5'None'                                              
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
*                                                                               
         XR    RE,RE                                                            
         C     RF,=F'360000'                                                    
         BL    SWT02                                                            
*                                                                               
         MVC   WORK2(5),=C'0H 00'  HOURS                                        
         D     RE,=F'360000'                                                    
         EDIT  (RF),(1,WORK2),ZERO=NOBLANK                                      
         LR    RF,RE                                                            
         XR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK2+3),FILL=0    MINS                                  
         B     EXITOK                                                           
*                                                                               
SWT02    MVC   WORK2(5),=C'00:00'                                               
         D     RE,=F'360000'                                                    
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK2),FILL=0    MINS                                    
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK2+3),FILL=0    SECS                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* HELP DISPLAY ROUTINE FOR ACTION FIELD                               *         
* ASSUMES THE ACTIONS WILL ALWAYS FIT ON A SCREEN                     *         
***********************************************************************         
P1HELP   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RUNHEDH        CLEAR SCREEN                                   
         USING FHD,R2                                                           
         XR    RF,RF                                                            
P1H02    ICM   RF,1,FHLN                                                        
         BZ    P1H04                                                            
         AHI   RF,-(FHDAD+1)                                                    
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AHI   RF,-(FHDAD)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FHDA(0),FHDA                                                     
         OI    FHOI,FHOITR                                                      
         IC    RF,FHLN                                                          
         BXH   R2,RF,P1H02                                                      
*                                                                               
P1H04    LA    R2,RUNLINH                                                       
         L     R3,AACTTAB                                                       
         USING ACTTABD,R3                                                       
*                                                                               
         LA    R4,RUNXXXH          R4 = A(END OF SCREEN)                        
         XR    R5,R5               KEEP THIS CLEAR (MUST BE ODD)                
*                                                                               
P1H06    CLI   ACTUNAM,255         EOT                                          
         BE    P1H10                                                            
*                                                                               
         CLI   DDS,YES             DDS CAN DO ANYTHING                          
         BE    *+12                                                             
         CLI   ACTUSER,ACTUALL     OTHERWISE ONLY 'ALL' ACTIONS                 
         BNE   P1H08                                                            
*                                                                               
         MVC   FHDA(3),=CL3'>>>'   SET ACTION FIELD TO >>>                      
         IC    R5,FHLN                                                          
         AR    R2,R5                                                            
*                                                                               
         LH    RF,ACTLNAM                                                       
         AR    RF,RC                                                            
         MVC   FHDA(8),0(RF)                                                    
         IC    R5,FHLN                                                          
         AR    R2,R5                                                            
*                                                                               
P1H08    AHI   R3,ACTTABL                                                       
         B     P1H06                                                            
*                                                                               
P1H10    B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SPECIAL TO DISPLAY CONTENTS OF REPORT                               *         
***********************************************************************         
SELJOB   NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
         MVC   SWORK1,0(R1)                                                     
         SAM24                                                                  
*                                                                               
         LA    R4,RUNHEDH          TOP OF SCREEN                                
         USING FHD,R4                                                           
         XR    RF,RF                                                            
SJOB02   ICM   RF,1,FHLN           CLEAR WHOLE SCREEN                           
         BZ    SJOB04                                                           
         NI    FHAT,255-(FHATHI+FHATLO)                                         
         OI    FHOI,FHOITR                                                      
*                                                                               
         LHI   R0,(FHDAD+1)                                                     
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         LHI   R0,(FHDAD+FHDAD+1)                                               
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FHDA(0),FHDA                                                     
         IC    RF,FHLN                                                          
         BXH   R4,RF,SJOB02                                                     
*                                                                               
SJOB04   LA    R1,SWORK1                                                        
         BRAS  RE,PQSTASEL         SELECT THIS                                  
*                                                                               
         LA    R4,RUNHEDH          TOP OF SCREEN                                
         LA    R3,CIREC                                                         
         LA    R3,PQDATA1-PQRECD(R3)                                            
         TM    PQTYPE-PQRECD(R3),PQTYNEW                                        
         BO    *+8                                                              
         LA    R3,PQDATA1-PQRECD(R3)                                            
*                                                                               
SJOB06   XR    R1,R1                                                            
         ICM   R1,3,0(R3)          GET LENGTH                                   
         CHI   R1,3                                                             
         BL    SJOB12                                                           
         BE    SJOB14              IGNORE IF LEN=3                              
*                                                                               
         CLC   3(2,R3),=C'//'      DROP DD CARDS                                
         BNE   SJOB10                                                           
*                                                                               
         LR    RF,R1                                                            
         CHI   RF,4                                                             
         BL    SJOB10                                                           
         LR    RE,R3                                                            
*                                                                               
SJOB08   CLC   0(4,RE),=C' DD '                                                 
         BE    SJOB14                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,SJOB08                                                        
*                                                                               
SJOB10   CHI   R1,L'RUNHED+2                                                    
         BL    *+8                                                              
         LHI   R1,L'RUNHED+2                                                    
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),2(R3)       MOVE IN DATA                                 
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,0(R3)                                                       
         AR    R3,R1                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R4,RF                                                            
         CLI   FHLN,0                                                           
         BNE   SJOB06                                                           
*                                                                               
SJOB12   LA    R0,170                                                           
         B     INFO                                                             
*                                                                               
SJOB14   ICM   R1,3,0(R3)          SKIP THIS LINE                               
         AR    R3,R1                                                            
         B     SJOB06                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RELOAD A REPORT WITH IT'S JCL                            *         
***********************************************************************         
REQUEUE  NTR1  BASE=*,LABEL=*                                                   
         ST    R2,SAVER2           SAVE REAL POINTER                            
         LR    R2,R1                                                            
         USING TBJNTRY,R2                                                       
*                                                                               
         LA    RF,NDX              READ FIRST CI REC USING REPORT ID            
         USING UKRECD,RF                                                        
         XC    NDX,NDX                                                          
         MVC   UKKEY(7),TBJPQUSR                                                
         MVI   UKFLAG,UKFLNUM+UKFLCIA+UKFLCIR                                   
         GOTO1 ADMGR,DMCB,INDEX,PRTQUE,NDX,WORK,CIREC                           
         CLI   8(R1),0                                                          
         BNE   EDISK                                                            
         LA    RF,NDX              EXTRACT RETURN VALUES                        
         MVC   DSKADR,UKUSRINF                                                  
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),UKUSRINF+4                                           
         DROP  RF                                                               
*                                                                               
         XC    FULL,FULL           USE FULL AS AN ERROR FLAG                    
         XC    DMCB(4*6),DMCB      GET JOB TABLE HEADER FROM TABS               
         MVC   DMCB(4),=AL4(DTMONS)                                             
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         L     R3,=AL4(DTMONS)     SCAN THIS TABLE FOR SAVED JCL                
         N     R3,=X'00007FFF'                                                  
         SLL   R3,6                                                             
         LAM   AR3,AR3,TBLET                                                    
         SAC   512                                                              
         USING DMSPACED,R3                                                      
*                                                                               
         ICM   R4,15,DSPTEND       R4=A(END)                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,DSPTFRST      R3=A(TABLE)                                  
         N     R3,=X'3FFFFFFF'                                                  
*                                                                               
REQU010  CLC   CIREC(7),0(R3)      TEST REPORT KEY                              
         BE    REQU100             FOUND IT SO REPLACE JCL                      
         AH    R3,=H'4096'         JCL SAVED IN 4K PAGES                        
         CR    R3,R4                                                            
         BL    REQU010                                                          
         B     REQUXXX             NO JCL FOUND                                 
*                                                                               
REQU100  LA    RE,CIREC            REPLACE THE WHOLE 4K                         
         LH    RF,=H'4096'                                                      
         LR    R4,R3                                                            
         LR    R5,RF                                                            
         CPYA  AR4,AR3                                                          
         MVCL  RE,R4               TURN OFF THE JOBO FLAG                       
         NI    CIREC+(PQATTB-PQINDEX),X'FF'-PQATJOBO                            
*                                                                               
         LAM   AR4,AR4,ARZERO                                                   
         SAC   0                                                                
         GOTO1 ADMGR,DMCB,DMWRT,PRTQID,DSKADR,CIREC                             
         CLI   8(R1),0             TEST FOR ERRORS                              
         BNE   REQUXXX                                                          
*                                                                               
         LA    RF,NDX              TURN OFF JOBO FLAG IN INDEX ENTRY            
         USING UKRECD,RF                                                        
         XC    NDX,NDX                                                          
         MVC   UKKEY(7),TBJPQUSR                                                
         MVI   UKFLAG,UKFLNUM                                                   
         GOTO1 ADMGR,DMCB,INDEX,PRTQID,NDX,WORK,CIREC                           
         CLI   8(R1),0                                                          
         BNE   EDISK                                                            
         GOTO1 ADMGR,DMCB,JOUNSET,PRTQID,NDX,WORK,CIREC                         
         CLI   8(R1),0                                                          
         BNE   EDISK                                                            
         MVI   FULL,YES            FLAG WE DID IT OK                            
         DROP  RF                                                               
*                                                                               
REQUXXX  SAC   0                                                                
         XC    DMCB(4*6),DMCB      FREE TABLE                                   
         MVC   DMCB(4),=AL4(DTMONS)                                             
         OI    DMCB,X'10'                                                       
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         CLI   FULL,YES            WERE WE SUCCESSFUL                           
         BNE   EXITL                                                            
*                                                                               
         BRAS  RE,LOCKCLS          LOCK CLASS TABLE                             
*                                                                               
         L     R3,=AL4(DTDCLASS)                                                
         N     R3,=X'00007FFF'                                                  
         SLL   R3,6                                                             
         LAM   AR3,AR3,TBLET                                                    
         SAC   512                                                              
         USING DMSPACED,R3                                                      
         ICM   R1,15,DSPTEND       R3=A(EOT)                                    
         ST    R1,ACLLAST                                                       
         ICM   R3,15,DSPTFRST      R3=A(TABLE)                                  
         N     R3,=X'3FFFFFFF'                                                  
         USING JCLASSD,R3                                                       
         CPYA  AR2,AR3                                                          
         L     R2,SAVER2                                                        
         USING TBJOBTAB,R2                                                      
         MVI   TBJSTAT,TBJINUSE                                                 
         XC    TBJRTIME,TBJRTIME                                                
*                                                                               
FCL04    OC    JCLASSD(JCKEYL),JCLASSD                                          
         BZ    FCL08                                                            
         CLC   JCCLASS,TBJCLASS    CLASS                                        
         BNE   FCL06                                                            
         B     FCL08                                                            
*                                                                               
FCL06    AHI   R2,JCLASSL          NEXT CLASS                                   
         C     R2,ACLLAST                                                       
         BL    FCL04                                                            
         DC    H'0'                CLASS TABLE IS FULL - NOT GOOD               
*                                                                               
FCL08    MVC   JCCLASS,TBJCLASS                                                 
*                                                                               
         XR    R0,R0               INCREMENT # SUB                              
         ICM   R0,3,JCNSUB                                                      
         AHI   R0,1                                                             
         STCM  R0,3,JCNSUB                                                      
         XR    R0,R0               INCREMENT TOTAL # SUB                        
         ICM   R0,3,JCNTSUB                                                     
         AHI   R0,1                                                             
         STCM  R0,3,JCNTSUB                                                     
*                                                                               
         OC    JCFSUB,JCFSUB       ANYTHING IN QUEUE?                           
         BNZ   FCL10               YES                                          
         STCM  R2,15,JCFSUB                                                     
         STCM  R2,15,JCLSUB                                                     
         B     FILLCLSX                                                         
*                                                                               
FCL10    LR    R0,R2               SAVE A(THIS ENTRY)                           
         ICM   R2,15,JCLSUB        LINKED LIST IS OK?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R0,15,JCLSUB        SET A(END OF LIST)                           
         STCM  R0,15,TBJNXT        SET A(NEXT IN LIST)                          
*                                                                               
FILLCLSX BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREECLS                                                       
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MANIPULATE PQ ENTRIES                                    *         
* NTRY: R1 = TABSJOB ENTRY TO PURGE                                   *         
***********************************************************************         
PQSTAPUR MVC   DUB,PURGE           PURGE THIS REPORT                            
         J     PQSTAT1                                                          
PQSTAHOL MVC   DUB,HOLD            HOLD THIS REPORT                             
         J     PQSTAT1                                                          
PQSTAACT MVC   DUB,ACTV            ACTIVATE THIS REORT                          
         J     PQSTAT1                                                          
PQSTASEL MVC   DUB,SELECT          SELECT THIS REORT                            
         J     PQSTAT1                                                          
*                                                                               
PQSTAT1  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1                                                            
         USING TBJNTRY,R2                                                       
         LA    RF,NDX              READ INDEX ENTRY USING REPORT ID             
         USING UKRECD,RF                                                        
         XC    NDX,NDX                                                          
         MVC   UKKEY(7),TBJPQUSR                                                
         MVI   UKFLAG,UKFLNUM                                                   
         GOTO1 ADMGR,DMCB,INDEX,PRTQUE,NDX,WORK,CIREC                           
         CLI   8(R1),0                                                          
         BNE   EDISK                                                            
         LA    RF,NDX              EXTRACT RETURN VALUES                        
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),UKUSRINF+4                                           
*                                                                               
PQSTAT2  CLC   DUB,SELECT          SELECT THIS REPORT                           
         BNE   PQSTAT3                                                          
         GOTO1 ADMGR,DMCB,READ,PRTQID,NDX,WORK,CIREC                            
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         B     EDISK                                                            
*                                                                               
PQSTAT3  TM    UKATTB-UKRECD(RE),UKATJOBI+UKATJOBO                              
         BO    EXITL                                                            
         TM    UKTYPE-UKRECD(RE),UKTYUPDT                                       
         BO    EXITL                                                            
         GOTO1 ADMGR,DMCB,DUB,PRTQID,NDX,WORK,CIREC                             
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         B     EDISK               PQ ERROR                                     
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PURGE A JOB ENTRY FROM JOB TABLE IN DATASPACE            *         
***********************************************************************         
PRGE     NTR1  BASE=*,LABEL=*                                                   
         CLI   DDS,YES             ONLY DDS CAN PURGE A JOB ENTRY               
         BNE   EIAC                                                             
         BRAS  RE,LOCKBOTH                                                      
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTFRST                                                
         AH    R2,JOBHDRL                                                       
         SAC   512                                                              
         USING TBJOBTAB,R2         R2 = TABS DATASPACE JOB TABLE                
*                                                                               
PJB02    OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BZ    PJB04                                                            
         CLC   TBJCLASS,EFFS                                                    
         BE    PJB04                                                            
         CLI   TBJSTAT,0                                                        
         BE    PJB04                                                            
         OC    TBJTERM,TBJTERM     THERE MUST BE A TERMINAL NUMBER. . .         
         BZ    PJB04                                                            
         OC    TBJPQKEY,TBJPQKEY   . . . AND REPORT ID. . .                     
         BZ    PJB04                                                            
*                                                                               
         OC    JOBUSER,JOBUSER     USERID FILTER                                
         BZ    *+14                                                             
         CLC   JOBUSER,TBJPQUSR                                                 
         BNE   PJB04                                                            
*                                                                               
         OC    JOBAGY,JOBAGY       AGY FILTER                                   
         BZ    *+14                                                             
         CLC   JOBAGY,TBJAGY                                                    
         BNE   PJB04                                                            
*                                                                               
         OC    JOBSUBID,JOBSUBID   SUB-ID FILTER                                
         BZ    *+14                                                             
         CLC   JOBSUBID,TBJPQSUB                                                
         BNE   PJB04                                                            
*                                                                               
         OC    JOBREPNO,JOBREPNO   REPORT # FILTER                              
         BZ    *+14                                                             
         CLC   JOBREPNO,TBJPQSEQ                                                
         BNE   PJB04                                                            
*                                                                               
         OC    TBJMONS,TBJMONS     MUST NOT BE RUNNING                          
         BZ    PJB06               PURGE THIS JOB                               
         BRAS  RE,ARSOFF                                                        
         B     EPUR                CANNOT PURGE THIS JOB                        
*                                                                               
PJB04    AH    R2,JOBTABL                                                       
         C     R2,JT.DSPTEND       DON'T RUN OFF TABLE END                      
         BL    PJB02                                                            
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEBOTH                                                      
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,WTO1                                                          
         B     EXITOK                                                           
*                                                                               
PJB06    MVI   TBJSTAT,0           FLAG IT FOR DELETION NEXT TIME               
         MVC   SWORK1,0(R2)                                                     
         ST    R2,FULL                                                          
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,INACLS           TAKE OUT OF LIST                             
*                                                                               
         LA    R1,SWORK1                                                        
         BRAS  RE,PQSTAPUR         PURGE THIS ON THE PQ                         
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEBOTH                                                      
         BRAS  RE,ARSOFF                                                        
*        BRAS  RE,WTO1                                                          
         BRAS  RE,WTOP                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SCAN CLASS TABLE TO CHECK IF THIS IN A SUBMITTED QUEUE              *         
* THEN REMOVE IT IF IT IS                                             *         
* NTRY: FULL   = A(JOBTAB ENTRY)                                      *         
***********************************************************************         
INACLS   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
         ICM   R2,15,CL.DSPTFRST    CLASS TABLE                                 
         LAM   AR2,AR2,TBLET                                                    
         USING JCLASSD,R2                                                       
         SAC   512                                                              
         CPYA  AR3,AR2                                                          
         L     R3,FULL                                                          
         USING TBJOBTAB,R3                                                      
*                                                                               
INCL02   OC    JCLASSD(JCKEYL),JCLASSD                                          
         BNZ   INCL04                                                           
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
*                                                                               
INCL04   ICM   R3,15,JCFSUB        ANY SUBMITTED JOBS HERE?                     
         BZ    INCL08              NO                                           
*                                                                               
INCL06   CLM   R3,15,FULL          MATCH MY ADDRESS                             
         BE    INCL10              HERE I BE                                    
         ICM   R3,15,TBJNXT                                                     
         BNZ   INCL06                                                           
*                                                                               
INCL08   AHI   R2,JCLASSL          NEXT CLASS ENTRY                             
         B     INCL02                                                           
*                                                                               
INCL10   XR    R0,R0                                                            
         ICM   R0,3,JCNSUB         REDUCE SUBMITTED COUNT                       
         BZ    *+8                                                              
         AHI   R0,-1                                                            
         STCM  R0,3,JCNSUB                                                      
*                                                                               
         CLC   JCFSUB,FULL         AM I FIRST SUBMIITED JOB?                    
         BNE   INCL12              NO                                           
         MVC   JCFSUB,TBJNXT       SET NEXT AS FIRST                            
         CLC   JCLSUB,FULL         AM I ALSO LAST SUBMITTED JOB?                
         BNE   INCL18              NO                                           
         XC    JCLSUB,JCLSUB       CLEAR LAST SUBMITTED                         
*                                                                               
         OC    JCFSUB,JCFSUB       THIS SHOULD BE ZERO NOW                      
         BZ    INCL18                                                           
         LHI   R0,1                LINKS IN TABLE ARE BROKEN                    
         B     REBUILD                                                          
*                                                                               
INCL12   CPYA  AR4,AR3                                                          
         ICM   R4,15,JCFSUB        FIRST SUBMITTED JOB                          
PS       USING TBJOBTAB,R4         PS = PRIOR SUBMITTED JOB                     
*                                                                               
INCL14   CLM   R3,15,PS.TBJNXT     GET JOB THAT POINTS TO ME                    
         BE    INCL16                                                           
         ICM   R4,15,PS.TBJNXT                                                  
         BNZ   INCL14                                                           
         LHI   R0,2                                                             
         B     REBUILD                                                          
*                                                                               
INCL16   MVC   PS.TBJNXT,TBJNXT    FIX UP LINKS (TAKE ME OUT)                   
         XC    TBJNXT,TBJNXT                                                    
*                                                                               
         CLM   R3,15,JCLSUB        WAS I THE LAST SUBMITTED?                    
         BNE   *+8                 NO                                           
         STCM  R4,15,JCLSUB        FIX LIST TAIL                                
         BRAS  RE,ARSOFF                                                        
         B     EXITOK              SET CC OK IF MATCH IN TABLE                  
         DROP  PS                                                               
*                                                                               
INCL18   BRAS  RE,ARSOFF                                                        
         B     EXITOK              SET CC LOW IF NO MATCH                       
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SET NEW PRIORITIES BASED ON INPUT DETAILS                           *         
***********************************************************************         
PRIORITY NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOCKJOB                                                       
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTFRST                                                
         AH    R2,JOBHDRL                                                       
         SAC   512                                                              
         USING TBJOBTAB,R2         R2 = TABS DATASPACE JOB TABLE                
*                                                                               
PRI02    OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BZ    PRI04                                                            
         CLC   TBJCLASS,EFFS                                                    
         BE    PRI04                                                            
         CLI   TBJSTAT,0                                                        
         BE    PRI04                                                            
         OC    TBJTERM,TBJTERM     THERE MUST BE A TERMINAL NUMBER. . .         
         BZ    PRI04                                                            
         OC    TBJPQKEY,TBJPQKEY   . . . AND REPORT ID. . .                     
         BZ    PRI04                                                            
*                                                                               
         OC    JOBUSER,JOBUSER     USERID FILTER                                
         BZ    *+14                                                             
         CLC   JOBUSER,TBJPQUSR                                                 
         BNE   PRI04                                                            
*                                                                               
         OC    JOBAGY,JOBAGY       AGY FILTER                                   
         BZ    *+14                                                             
         CLC   JOBAGY,TBJAGY                                                    
         BNE   PRI04                                                            
*                                                                               
         OC    JOBSUBID,JOBSUBID   SUB-ID FILTER                                
         BZ    *+14                                                             
         CLC   JOBSUBID,TBJPQSUB                                                
         BNE   PRI04                                                            
*                                                                               
         OC    JOBREPNO,JOBREPNO   REPORT # FILTER                              
         BZ    *+14                                                             
         CLC   JOBREPNO,TBJPQSEQ                                                
         BNE   PRI04                                                            
*                                                                               
         MVC   TBJPRTY,JOBPRI      SET NEW PRIORITY                             
*                                                                               
PRI04    AH    R2,JOBTABL                                                       
         C     R2,JT.DSPTEND       DON'T RUN OFF TABLE END                      
         BL    PRI02                                                            
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEJOB                                                       
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,WTO                                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE                                                          *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVI   LONGS,C'A'          DEFAULT TO EVERYTHING                        
*                                                                               
         L     RF,SRPATIOB                                                      
         USING TIOBD,RF            RF=A(TIOB)                                   
         MVC   PFKEY,TIOBAID       PF KEY NUMBER                                
         MVC   CURSORD,TIOBCURS                                                 
*                                                                               
         LA    R1,RUNHEDH          IF CURSOR < RUNHED                           
         CLC   CURSORD,2(R1)                                                    
         BH    *+8                                                              
         MVI   FLAG,C'I'           REFRESH                                      
*                                                                               
         L     RF,SRPASYS          EXTRACT FROM SYSFACS                         
         USING SYSFACD,RF                                                       
         MVC   MYSSB,VSSB                                                       
         MVC   ADMGR,VDATAMGR                                                   
         MVC   ADMOD000,VDMOD000                                                
         MVC   ACALLOV,VCALLOV                                                  
         MVC   ATICTOC,VTICTOC                                                  
         MVC   ALOCKSPC,VLOCKSPC                                                
         MVC   AWCTYPE,VWCTYPE                                                  
*                                                                               
         L     RF,MYSSB            EXTRACT FROM SSB                             
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL                                                   
         MVC   TBLET,SSBTBLET                                                   
         MVC   SYSID,SSBSYSID                                                   
         MVC   AFID,SSBAFID                                                     
         MVC   ATCB,SSBTKADR                                                    
*                                                                               
         L     RF,ATCB                                                          
         USING TCBD,RF                                                          
         MVC   ABIGBUF,TCBTSAR                                                  
*                                                                               
         L     RF,SRPAUTL          EXTRACT FROM UTL                             
         USING UTLD,RF                                                          
         MVC   TERMSTAT,TSTAT                                                   
         MVC   TRANSNUM,TTRCNT                                                  
         MVC   MYLUID,TSYM                                                      
         MVC   MYUSER,TUSER                                                     
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+8                                                              
         OI    TSVCREQ,X'02'       SET =XRSOR FLAG                              
*                                                                               
         L     RF,SRPACOM          EXTRACT FROM COMFACS                         
         USING COMFACSD,RF                                                      
         MVC   VSCANNER,CSCANNER                                                
         MVC   VTERMVAL,CTERMVAL                                                
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VDICTATE,CDICTATE                                                
         MVC   VGETHELP,CGETHELP                                                
         MVC   VGETTXT,CGETTXT                                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 ACALLOV,DMCB,0,X'D9000A0D',0                                     
         MVC   VSQUASH,0(R1)                                                    
*                                                                               
         XR    RF,RF               SET 13-24 TO 1-12                            
         IC    RF,PFKEY                                                         
         CHI   RF,12                                                            
         BNH   *+8                                                              
         AHI   RF,-12                                                           
         STC   RF,PFKEY                                                         
*                                                                               
         L     RF,AFID                                                          
         XR    R0,R0                                                            
         IC    R0,SYSID                                                         
         MHI   R0,L'FACITAB                                                     
         AR    RF,R0                                                            
         USING FACITABD,RF                                                      
         MVC   SYSIDFL,FACIFL      SAVE THIS FACIFL                             
         DROP  RF                                                               
*                                                                               
         BRAS  RE,ENQJOB           GET DATASPACE HEADERS                        
         BRAS  RE,ENQCLS                                                        
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTEND                                                 
         ST    R2,AJOBTABX                                                      
         ICM   R2,15,JT.DSPTFRST                                                
         ST    R2,AJOBTAB                                                       
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         MVC   JOBHDRL,TBJLHEAD    SET SOFT LENGTHS                             
         MVC   JOBTABL,TBJLNTRY                                                 
         BRAS  RE,ARSOFF                                                        
         DROP  R2                                                               
*                                                                               
         NI    RUNACTH+6,X'FF'-X'40'                                            
         MVCDD RUNHED,SR#RUN04     SET UP DEFAULT HEADERS                       
         MVCDD RUNHEDU,SR#RUN05                                                 
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LU  ',DCLIST,DSLISTU                             
         GOTO1 (RF),(R1),C'LL  ',DCLIST,DSLISTL                                 
*                                                                               
         MVC   UC@SUMUK,DC@SUMUK                                                
         MVC   UC@RESET,DC@RESET                                                
*                                                                               
         MVC   HELPKEY,HELPID      SET UP HELP ID                               
*                                                                               
         MVI   DDS,YES             DDS CAN SAY ,CLI                             
         TM    TERMSTAT,TSTATDDS                                                
         BZ    INIT02                                                           
         CLC   =C'RUN,CLI',RUNSRV+1   =C'=RUN,CLI'                              
         BNE   *+14                                                             
INIT02   MVI   DDS,NO              NOT DDS                                      
         MVC   JOBUSER,MYUSER      USE MY USERID                                
*                                                                               
         L     R1,=A(ACTTAB)       RELOCATE TABLE ADDRESSES                     
         A     R1,RELO                                                          
         ST    R1,AACTTAB                                                       
*&&US*&& L     R1,=A(ACTSUM)       SET DEFAULT ACTION                           
*&&UK*&& L     R1,=A(ACTLIS)                                                    
         A     R1,RELO                                                          
         ST    R1,AACTDEF                                                       
         L     R1,=A(OPTTAB)                                                    
         A     R1,RELO                                                          
         ST    R1,AOPTTAB                                                       
         L     R1,=A(SUBTAB)                                                    
         A     R1,RELO                                                          
         ST    R1,ASUBTAB                                                       
         L     R1,=A(STATAB)                                                    
         A     R1,RELO                                                          
         ST    R1,ASTATAB                                                       
*                                                                               
         ICM   R0,7,=XL3'000A50'                                                
         ICM   R0,8,=C'R'                                                       
         GOTO1 ACALLOV,DMCB,0,(R0),0                                            
         MVC   VQSORT,0(R1)                                                     
         OI    VQSORT,X'80'                                                     
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'01',0)                                          
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         ICM   R0,15,FATIME                                                     
         MHI   R0,100              TIME BIN PLEASE                              
         ST    R0,CURTIME          CURRENT BINARY TIME                          
         DROP  R1                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY JCL BUFFER                                       *         
***********************************************************************         
LISTJCL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        LOAD  SCREEN AND HEADERS                                               
*                                                                               
         GOTO1 ACALLOV,DMCB,RUNLINH,X'D90158FE',0                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,REMERGE                                                       
*                                                                               
         LH    R1,ACT.ACTUNAM      MAKE SURE ACTION STAYS                       
         AR    R1,RC                                                            
         MVC   RUNACT(8),0(R1)                                                  
*                                                                               
         MVC   RUNHED,SPACES                                                    
         MVC   RUNHEDU,SPACES                                                   
         MVC   RUNHED(77),LISTL1                                                
         MVC   RUNHEDU(77),LISTL2                                               
*                                                                               
         LA    R4,RUNSELH                                                       
         USING LSTLINED,R4                                                      
*                                                                               
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTMONS)                                             
         MVI   DMCB,X'20'                                                       
         GOTO1 ALOCKSPC,DMCB                                                    
         L     RF,4(R1)                                                         
         ICM   R2,15,DSPTFRST-DMSPACED(RF)                                      
         N     R2,=X'3FFFFFFF'                                                  
         ST    R2,FULL                                                          
         ICM   R3,15,DSPTEND-DMSPACED(RF)                                       
*                                                                               
         CLI   PFKEY,7             PF7 = UP                                     
         BNE   LISTJ004                                                         
*                                                                               
         L     R2,RUNSSTOP         SET NEW STARTPOINT                           
         S     R2,=A(4096*19)                                                   
         ST    R2,RUNSSBOT                                                      
         ICM   R2,15,FULL                                                       
*                                                                               
LISTJ004 C     R2,RUNSSBOT                                                      
         BNL   *+8                                                              
         L     R2,RUNSSBOT                                                      
*                                                                               
         CR    R2,R3                                                            
         BL    *+8                                                              
         ICM   R2,15,FULL                                                       
         ST    R2,RUNSSTOP                                                      
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING PQRECD,R2                                                        
*                                                                               
LISTJ010 MVC   HALF,PQSRCID        USER NAME                                    
         SAC   0                                                                
         BRAS  RE,GETUNAM                                                       
         SAC   512                                                              
         MVC   LSTUSER,WORK2                                                    
*                                                                               
         MVC   LSTREPT(3),PQSUBID                                               
         MVI   LSTREPT+3,C','                                                   
         XR    R0,R0                                                            
         ICM   R0,3,PQREPNO                                                     
         EDIT  (R0),(5,LSTREPT+4),ALIGN=LEFT                                    
*                                                                               
         SR    RF,RF               TIME FROM REPORT                             
         SR    RE,RE                                                            
         ICM   RF,3,PQAGELT                                                     
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
*                                                                               
         A     RF,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         BRAS  RE,SHOWTIME                                                      
         MVC   LSTTIME,WORK2                                                    
*                                                                               
         CPYA  AR5,AR2                                                          
         LA    R5,PQDATA1                                                       
         TM    PQTYPE,PQTYNEW                                                   
         BO    *+8                                                              
         LA    R5,PQDATA1                                                       
LISTJ020 XR    R0,R0                                                            
         ICM   R0,3,0(R5)          TEST FOR BAD REPORT                          
         BZ    LISTJ040            DONT CORE LOOP                               
*                                                                               
         CHI   R0,3                SKIP TAB FIELDS                              
         BE    LISTJ030                                                         
         CHI   R0,1                0001 = END OF PAGE                           
         BE    LISTJ030                                                         
*                                                                               
         CLC   3(2,R5),=C'//'      IS THIS A JCL CARD                           
         BNE   LISTJ030                                                         
         CLC   13(5,R5),=C' JOB '  IS THIS A JOB CARD                           
         BNE   *+10                                                             
         MVC   LSTJOBN,5(R5)                                                    
*                                                                               
LISTJ030 LTR   R0,R0               IF R0 IS NEGATIVE GET OUT                    
         BM    LISTJ040            THIS HAS CAUSED A CORE LOOP                  
         AR    R5,R0                                                            
         B     LISTJ020                                                         
*                                                                               
LISTJ040 AHI   R2,4096             NEXT JCL BLOCK                               
         CR    R2,R3                                                            
         BNL   LISTJ050            UNTIL END                                    
         LH    R1,SEQDIS                                                        
         AHI   R1,1                                                             
         STH   R1,SEQDIS                                                        
         LA    R4,93(R4)                                                        
         LA    R1,RUNXX2H          OR END OF SCREEN                             
         CR    R4,R1                                                            
         BL    LISTJ010                                                         
*                                                                               
LISTJ050 ST    R2,RUNSSBOT                                                      
         LAM   AR5,AR5,ARZERO                                                   
         L     RF,RUNSSTOP         TOP OFFSET                                   
         SRL   RF,12               DIVIDE BY 4096                               
         LA    RF,1(RF)                                                         
         STH   RF,SEQLO                                                         
*                                                                               
         L     RF,RUNSSBOT         BOTTOM OFFSET                                
         SRL   RF,12               DIVIDE BY 4096                               
         STH   RF,SEQHI                                                         
         SAC   0                                                                
         SAM24                                                                  
         CR    RB,RB                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY JCL FROM BUFFER                                  *         
***********************************************************************         
DISPJCL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        LOAD  SCREEN AND HEADERS                                               
*                                                                               
         GOTO1 ACALLOV,DMCB,RUNLINH,X'D90158FE',0                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,REMERGE                                                       
*                                                                               
         LH    R1,ACT.ACTUNAM      MAKE SURE ACTION STAYS                       
         AR    R1,RC                                                            
         MVC   RUNACT(8),0(R1)                                                  
*                                                                               
         MVC   RUNHED,SPACES                                                    
         MVC   RUNHEDU,SPACES                                                   
         MVC   RUNHED(77),LISTL1                                                
         MVC   RUNHEDU(77),LISTL2                                               
*                                                                               
         LA    R4,RUNSELH                                                       
         USING LSTLINED,R4                                                      
*                                                                               
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTMONS)                                             
         MVI   DMCB,X'20'                                                       
         GOTO1 ALOCKSPC,DMCB                                                    
         L     RF,4(R1)                                                         
         ICM   R2,15,DSPTFRST-DMSPACED(RF)                                      
         N     R2,=X'3FFFFFFF'                                                  
         ST    R2,FULL                                                          
         ICM   R3,15,DSPTEND-DMSPACED(RF)                                       
*                                                                               
         SAC   512                                                              
WRK      USING TBJNTRY,SWORK1                                                   
DISPJ010 CLC   WRK.TBJPQKEY,0(R2)                                               
         DROP  WRK                                                              
         BE    DISPJ020                                                         
         AHI   R2,4096                                                          
         CR    R2,R3                                                            
         BL    DISPJ010                                                         
         B     EXITOK                                                           
*                                                                               
DISPJ020 LA    R2,PQDATA1-PQRECD(R2)                                            
*                                                                               
         LA    R0,1                                                             
         CLI   SELFULL+1,C'2'                                                   
         BNE   *+8                                                              
         LA    R0,20                                                            
         CLI   SELFULL+1,C'3'                                                   
         BNE   *+8                                                              
         LA    R0,40                                                            
*                                                                               
DISPJ030 XR    R1,R1                                                            
         ICM   R1,3,0(R2)                                                       
         AR    R2,R1                                                            
         BCT   R0,DISPJ030                                                      
         SR    R2,R1                                                            
*                                                                               
         SHI   R1,3                                                             
         BM    DISPJ040                                                         
*                                                                               
         LR    RF,R1                                                            
         CHI   RF,73                                                            
         BL    *+8                                                              
         LA    RF,73                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LSTLIN(0),3(R2)                                                  
DISPJ040 LA    R2,3(R1,R2)                                                      
*                                                                               
         CLC   LSTSEL(2),=C'J3'                                                 
         BNE   *+10                                                             
         MVC   LSTSEL(2),=C'  '                                                 
         CLC   LSTSEL(2),=C'J2'                                                 
         BNE   *+10                                                             
         MVC   LSTSEL(2),=C'J3'                                                 
         CLC   LSTSEL(2),=X'D100'  = J                                          
         BNE   *+10                                                             
         MVC   LSTSEL(2),=C'J2'                                                 
*                                                                               
         LA    R4,93(R4)                                                        
         LA    R1,RUNXX2H                                                       
         LA    R0,1                                                             
         CR    R4,R1                                                            
         BL    DISPJ030                                                         
         B     DISPJCLX                                                         
*                                                                               
DISPJCLX B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIST SEL RUN SCREEN                              *         
***********************************************************************         
LISTREP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        LOAD  SCREEN AND HEADERS                                               
*                                                                               
         GOTO1 ACALLOV,DMCB,RUNLINH,X'D90158FE',0                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,REMERGE                                                       
*                                                                               
         LH    R1,ACT.ACTUNAM      MAKE SURE ACTION STAYS                       
         AR    R1,RC                                                            
         MVC   RUNACT(8),0(R1)                                                  
*                                                                               
         MVC   RUNHED,SPACES                                                    
         MVC   RUNHEDU,SPACES                                                   
         MVC   RUNHED(77),LISTL1                                                
         MVC   RUNHEDU(77),LISTL2                                               
*                                                                               
         LA    R4,RUNSELH                                                       
         USING LSTLINED,R4                                                      
*                                                                               
         LA    R5,RUNSOFFS         OFFSET TAB                                   
*                                                                               
         BRAS  RE,BLDJOBT          COPY JOB TAB TO TSAR BUFFER                  
         MVC   SEQDIS,TABCOUNT+2                                                
*                                                                               
         L     R2,RUNSSTOP                                                      
         A     R2,ABIGBUF          R2 = A(XA BUFFER)                            
         USING TBJOBTAB,R2                                                      
         SAM31                                                                  
         SAC   0                                                                
*                                                                               
         CLI   FLAG,C'I'           INIT                                         
         BNE   *+12                                                             
         L     R2,ABIGBUF                                                       
         B     LIST005                                                          
*                                                                               
         CLI   PFKEY,5             PF5 TOP REQUEST                              
         BNE   *+8                                                              
         L     R2,ABIGBUF                                                       
*                                                                               
         CLI   PFKEY,6             PF6 BOTTOM REQUEST                           
         BNE   LIST002                                                          
         L     R2,TABCOUNT         TOTAL JOBS                                   
         SHI   R2,19               LESS 19 FOR A SCREEN FULL                    
         LTR   R2,R2                                                            
         BP    *+6                                                              
         SR    R2,R2               ZERO IF < 19                                 
         SLL   R2,6                MULTIPLY BY 64                               
         A     R2,ABIGBUF                                                       
*                                                                               
LIST002  CLI   PFKEY,7             PF7 UP REQUEST                               
         BNE   LIST004                                                          
         L     R1,RUNSSTOP                                                      
         A     R1,ABIGBUF                                                       
         SH    R1,=Y(19*L'TBJNTRY) BACK UP 19 JOBS                              
         LR    R2,R1                                                            
         C     R2,ABIGBUF                                                       
         BNL   LIST005                                                          
         L     R2,ABIGBUF                                                       
         B     LIST005                                                          
*                                                                               
LIST004  CLI   PFKEY,8             START FROM BOTTOM                            
         BNE   LIST005                                                          
         L     R2,RUNSSBOT                                                      
         A     R2,ABIGBUF                                                       
         OC    0(L'TBJNTRY,R2),0(R2)                                            
         BNZ   LIST005                                                          
         L     R2,RUNSSTOP         IF END JUST IGNORE IT                        
         A     R2,ABIGBUF                                                       
*                                                                               
LIST005  LR    R1,R2               SAVE OFFSET TO TOP ENTRY                     
         S     R1,ABIGBUF                                                       
         ST    R1,RUNSSTOP                                                      
*                                                                               
LIST010  ST    R4,AHELP                                                         
         CLI   LSTSEL,C' '         ANY ACTION                                   
         BNH   LIST014                                                          
         CLI   LSTSEL,C'X'         OLD ERROR                                    
         BE    LIST011                                                          
         CLI   LSTSEL,C'?'         OLD ERROR                                    
         BE    HELPOUT                                                          
         CLC   LSTSEL(2),=C'OK'    OLD COMMAND                                  
         BNE   LIST012                                                          
*                                                                               
LIST011  MVC   LSTSEL,=C'   '      REMOVE OLD ERROR OR COMMAND                  
         B     LIST014                                                          
*                                                                               
LIST012  CLC   0(2,R5),TBJOFS+2    SAME ENTRY                                   
         BNE   LIST013                                                          
*                                                                               
         LA    R1,LSTSEL           GO TO SELACT WITH R5 AND R1                  
         LR    R3,R2               AND R3 POINTING TO LOCAL COPY                
         BRAS  RE,SELACT                                                        
         BE    LIST014                                                          
*                                                                               
LIST013  MVC   LSTSEL,=C'XXX'      SHOW NOT ACTIONED                            
*                                                                               
LIST014  MVC   0(2,R5),TBJOFS+2    SAVE OFFSET FOR LIST SEL                     
         LA    R5,2(R5)                                                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,TBJSTIME                                                    
         BZ    LIST990                                                          
         CLI   TBJSTIME,X'FF'      DISPLAY TIME REBUILT BY RERUN?               
         BNE   LIST015                                                          
         N     RF,=X'0000FFFF'                                                  
         XR    RE,RE                                                            
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
*                                                                               
LIST015  A     RF,DDSTIME          CORRECT DDS TIME TO REAL TIME                
         BRAS  RE,SHOWTIME                                                      
         MVC   LSTTIME,WORK2                                                    
*                                                                               
         MVC   HALF,TBJPQUSR       USER NAME                                    
         BRAS  RE,GETUNAM                                                       
         MVC   LSTUSER,WORK2                                                    
*                                                                               
         MVC   LSTREPT(3),TBJPQSUB                                              
         MVI   LSTREPT+3,C','                                                   
         XR    R0,R0                                                            
         ICM   R0,3,TBJPQSEQ                                                    
         EDIT  (R0),(5,LSTREPT+4),ALIGN=LEFT                                    
*                                                                               
         MVC   BYTE,TBJADV                                                      
         NI    BYTE,X'0F'                                                       
         XR    RF,RF                                                            
         IC    RF,BYTE                                                          
         MHI   RF,L'FACITAB                                                     
         A     RF,AFID                                                          
         USING FACITABD,RF                                                      
         MVC   LSTFAC,FACISN4                                                   
         DROP  RF                                                               
*                                                                               
         MVC   LSTTYPE,=CL2'  '                                                 
         MVC   LSTTYPE(1),TBJCTYPE                                              
         TM    TBJSTAT,JOBFUPDT                                                 
         BZ    *+8                                                              
         MVI   LSTTYPE+1,C'U'      INDICATE UPDATIVE                            
*                                                                               
         EDIT  (B1,TBJPRTY),LSTPRTY                                             
*                                                                               
         MVC   LSTCLASS,TBJCLASS                                                
         MVC   LSTJOBN,TBJMVSID                                                 
         MVC   LSTTERM,TBJLUID                                                  
         MVC   LSTMONS,TBJMONS                                                  
*                                                                               
         L     R1,ASTATAB                                                       
         LA    RF,=CL6'??????'                                                  
LIST020  CLI   0(R1),0             TEST E-O-T                                   
         BE    LIST025                                                          
         MVC   BYTE,TBJSTAT                                                     
         NI    BYTE,255-TBJIGNOR                                                
         NC    BYTE,4(R1)                                                       
         BNZ   *+12                                                             
         AHI   R1,L'STATAB                                                      
         B     LIST020                                                          
*                                                                               
         EX    0,0(R1)                                                          
*                                                                               
LIST025  MVC   LSTSTAT,0(RF)                                                    
         NC    LSTSTAT+1(6),=X'BFBFBFBFBFBF'                                    
*                                                                               
         MVI   LSTIGNOR,C' '                                                    
         TM    TBJSTAT,TBJIGNOR                                                 
         BZ    *+8                                                              
         MVI   LSTIGNOR,C'+'                                                    
*                                                                               
         CLI   TBJPRTY,X'F0'                                                    
         BNE   *+8                                                              
         MVI   LSTRUNOW,C'*'       LSTRUNOW = LSTIGNOR FIELD                    
*                                                                               
*        TM    TBJSTAT,TBJRUNOW                                                 
*        BZ    *+8                                                              
*        MVI   LSTRUNOW,C'*'       LSTRUNOW = LSTIGNOR FIELD                    
*                                                                               
         TM    TBJSTAT,JOBFRUN     ONLY IF RUNNING                              
         BZ    LIST030                                                          
*                                                                               
         OC    TBJMONS,TBJMONS                                                  
         BNZ   *+10                                                             
         MVC   LSTMONS,=C'WAITING'                                              
*                                                                               
         XR    R0,R0               DISPLAY TIME RUNNING                         
         ICM   R0,7,TBJRTIME                                                    
         BZ    LIST030                                                          
         L     RF,CURTIME                                                       
         SR    RF,R0                                                            
         BNP   LIST030                                                          
         BRAS  RE,SHOWWAIT                                                      
         MVC   LSTSTAT,SPACES                                                   
         MVC   LSTSTAT(5),WORK2                                                 
*                                                                               
LIST030  AH    R2,JOBTABL                                                       
         LA    R4,93(R4)                                                        
         LA    R1,RUNXX2H                                                       
         CR    R4,R1                                                            
         BL    LIST010                                                          
*                                                                               
LIST990  LR    R1,R2                                                            
         S     R1,ABIGBUF                                                       
         ST    R1,RUNSSBOT                                                      
*                                                                               
         L     RF,RUNSSTOP         TOP OFFSET                                   
         SRL   RF,6                DIVIDE BY 64                                 
         LA    RF,1(RF)                                                         
         STH   RF,SEQLO                                                         
*                                                                               
         L     RF,RUNSSBOT         BOTTOM OFFSET                                
         SRL   RF,6                DIVIDE BY 64                                 
         STH   RF,SEQHI                                                         
         SAC   0                                                                
         SAM24                                                                  
         CR    RB,RB                                                            
         B     EXITOK                                                           
         EJECT                                                                  
*************************************************************                   
*        PERFORM LIST SEL ACTIONS                           *                   
*************************************************************                   
SELACT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
SELAC2   LH    R2,0(R5)            LOCATE REAL ENTRY                            
         SLL   R2,6                                                             
         A     R2,AJOBTAB                                                       
*                                                                               
         L     RF,ASUBTAB          GET ACTION CODE                              
SELACT1  CLI   0(RF),255                                                        
         BE    EXITL                                                            
         CLC   0(1,R1),0(RF)                                                    
         BE    SELACT2                                                          
         LA    RF,4(RF)                                                         
         B     SELACT1                                                          
*                                                                               
SELACT2  MVC   BYTE,3(RF)          SAVE ACTION CODE                             
         MVC   0(3,R1),=C'OK '                                                  
*&&US                              ALLOW IT, YYUN, 1/28/08                      
*                                  OPS ONLY ACTION                              
         CLI   BYTE,13             RUN IT NOW                                   
         BNE   SELACT2A                                                         
         TM    TBJSTAT-TBJNTRY(R2),TBJUPDT                                      
         BO    SELACTN             NOT ALLOW FOR UPDATIVE SOON                  
         TM    TBJSTAT-TBJNTRY(R3),JOBFSUB                                      
         BZ    SELACTN             ONLY ALLOW FOR SOON NOT YET RUN              
         B     SELACT2B                                                         
*                                                                               
SELACT2A CLI   BYTE,3              UP/DOWN                                      
         BNL   SELACT2D                                                         
*                                                                               
SELACT2B B     SELACT2D       THERE IS NO OPS TERMINALS, YYUN, 12/8/09          
*                                                                               
         CLC   MYLUID(6),=C'D1LH00' ARE WE AN OPS TERMINAL                      
         BNE   SELACTNA                                                         
         LA    R0,13                                                            
         LA    RF,=C'0123456789ABC' IN THE RANGE 00T TO 0CT                     
SELACT2C CLC   MYLUID+6(1),0(RF)                                                
         BE    SELACT2E                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,SELACT2C                                                      
         B     SELACTNA                                                         
*&&                                                                             
SELACT2D CLI   BYTE,6              TEMP PRIORITY ACTIONS                        
         BL    SELACT2E            FOR TESTING                                  
         CLI   BYTE,7              TEMP PRIORITY ACTIONS                        
         BH    SELACT2E            FOR TESTING                                  
         SR    R0,R0                                                            
         IC    R0,BYTE                                                          
         SHI   R0,5                                                             
         STC   R0,BYTE                                                          
*                                                                               
SELACT2E LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
*                                                                               
SELACT3  CLI   BYTE,1              ACTION 1 UP                                  
         BNE   SELACT4                                                          
         SR    R1,R1                                                            
         ICM   R1,1,TBJPRTY-TBJNTRY(R2)                                         
         LA    R1,1(R1)                                                         
         CHI   R1,X'FA'                                                         
         BE    SELACTX                                                          
         STCM  R1,1,TBJPRTY-TBJNTRY(R2)                                         
         STCM  R1,1,TBJPRTY-TBJNTRY(R3)                                         
         B     SELACTX                                                          
*                                                                               
SELACT4  CLI   BYTE,2              ACTION 2 DOWN                                
         BNE   SELACT5                                                          
         SR    R1,R1                                                            
         ICM   R1,1,TBJPRTY-TBJNTRY(R2)                                         
         BCTR  R1,0                                                             
         CHI   R1,X'F0'                                                         
         BE    SELACTX                                                          
         STCM  R1,1,TBJPRTY-TBJNTRY(R2)                                         
         STCM  R1,1,TBJPRTY-TBJNTRY(R3)                                         
         B     SELACTX                                                          
*                                                                               
SELACT5  CLI   BYTE,3              ACTION 3 HOLD                                
         BNE   SELACT6                                                          
         OI    TBJSTAT-TBJNTRY(R2),TBJHOLD                                      
         MVI   TBJSTAT-TBJNTRY(R3),JOBFHOLD                                     
         B     SELACTX                                                          
*                                                                               
SELACT6  CLI   BYTE,4              ACTION 4 RELEASE                             
         BNE   SELACT7                                                          
         NI    TBJSTAT-TBJNTRY(R2),255-TBJHOLD                                  
         MVI   TBJSTAT-TBJNTRY(R3),JOBFSUB                                      
         TM    TBJSTAT-TBJNTRY(R2),TBJKILL                                      
         BZ    SELACTX                                                          
*                                                                               
         MVC   SWORK1,0(R2)                                                     
         BRAS  RE,ARSOFF                                                        
         LA    R1,SWORK1                                                        
         BRAS  RE,REQUEUE          RELOAD JCL R2 = A(ENTRY)                     
         BNZ   SELACTN                                                          
         B     SELACTX                                                          
*                                                                               
SELACT7  CLI   BYTE,5              ACTION 5 PURGE                               
         BE    SELACT7A                                                         
         CLI   BYTE,11             ACTION 11 Z  (PURGE FORCE)                   
         BE    SELACT7B                                                         
         BNE   SELACT8                                                          
*                                                                               
SELACT7A OC    TBJMONS-TBJNTRY(7,R2),TBJMONS-TBJNTRY(R2)                        
         BNZ   SELACTN             MUSTN'T BE RUNNING                           
SELACT7B BRAS  RE,LOCKBOTH         <-- PLAN B (DON'T CARE)                      
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         MVI   TBJSTAT-TBJNTRY(R2),0    FLAG FOR DELETION                       
         MVC   SWORK1,0(R2)                                                     
         ST    R2,FULL                                                          
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,INACLS           TAKE OUT OF LIST                             
*                                                                               
         LA    R1,SWORK1                                                        
         BRAS  RE,PQSTAPUR         PURGE THIS ON THE PQ                         
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEBOTH                                                      
         BRAS  RE,ARSOFF                                                        
*        BRAS  RE,WTO1                                                          
         BRAS  RE,WTOP                                                          
         B     EXITOK                                                           
*                                                                               
SELACT8  CLI   BYTE,8              ACTION 8 KILL                                
         BNE   SELACT9                                                          
         OI    TBJSTAT-TBJNTRY(R2),TBJKILL                                      
         OI    TBJSTAT-TBJNTRY(R3),JOBFKILL                                     
         B     SELACTX                                                          
*                                                                               
SELACT9  CLI   BYTE,9              ACTION 9 DISPLAY JCL                         
         BNE   SELACT10                                                         
*                                                                               
         MVC   SWORK1,0(R2)                                                     
         BRAS  RE,ARSOFF                                                        
         LA    R1,SWORK1                                                        
         BRAS  RE,DISPJCL          DISPLAY JCL                                  
         B     SELACTX                                                          
*                                                                               
SELACT10 CLI   BYTE,10             ACTION 10 CLEAR JOBTABLE ENTRY ONLY          
         BNE   SELACTA                                                          
*                                                                               
         BRAS  RE,LOCKBOTH                                                      
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         MVI   TBJSTAT-TBJNTRY(R2),0    FLAG FOR DELETION                       
         MVC   SWORK1,0(R2)                                                     
         ST    R2,FULL                                                          
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,INACLS           TAKE OUT OF LIST                             
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEBOTH                                                      
         BRAS  RE,ARSOFF                                                        
*        BRAS  RE,WTO1                                                          
         BRAS  RE,WTOC                                                          
         B     SELACTX                                                          
*                                                                               
SELACTA  CLI   BYTE,12             ACTION 12 IGNORE                             
         BNE   SELACTB                                                          
         OI    TBJSTAT-TBJNTRY(R2),TBJIGNOR                                     
         OI    TBJSTAT-TBJNTRY(R3),TBJIGNOR                                     
         B     SELACTX                                                          
*                                                                               
SELACTB  CLI   BYTE,13             ACTION 13 RUN IT NOW                         
         BNE   SELACTC                                                          
         OI    TBJSTAT-TBJNTRY(R2),TBJRUNOW                                     
         MVI   TBJPRTY-TBJNTRY(R2),X'F0'                                        
         MVI   TBJPRTY-TBJNTRY(R3),X'F0'                                        
*        OI    TBJSTAT-TBJNTRY(R3),TBJRUNOW                                     
         B     SELACTX                                                          
*                                                                               
SELACTC  CLI   BYTE,14             ACTION 14 RESET STUCK COMSCORE REP           
         BNE   SELACTN                                                          
*                                                                               
         NI    TBJSTAT-TBJNTRY(R2),X'FF'-TBJHOLD                                
         XC    TBJRTIME-TBJNTRY(L'TBJRTIME,R2),TBJRTIME-TBJNTRY(R2)             
         NI    TBJSTAT-TBJNTRY(R3),X'FF'-TBJHOLD                                
         XC    TBJRTIME-TBJNTRY(L'TBJRTIME,R3),TBJRTIME-TBJNTRY(R3)             
*                                                                               
         USING TBJNTRY,R2                                                       
*                                                                               
         LA    RF,NDX              READ FIRST CI REC USING REPORT ID            
         USING UKRECD,RF                                                        
         XC    NDX,NDX                                                          
         MVC   UKKEY(7),TBJPQUSR                                                
         MVI   UKFLAG,UKFLNUM+UKFLCIA+UKFLCIR                                   
*                                                                               
         SAC   0                                                                
*                                                                               
         GOTO1 ADMGR,DMCB,INDEX,PRTQUE,NDX,WORK,CIREC                           
         CLI   8(R1),0                                                          
         BNE   EDISK                                                            
*                                                                               
         LA    RF,NDX              EXTRACT RETURN VALUES                        
         MVC   DSKADR,UKUSRINF                                                  
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),UKUSRINF+4                                           
         DROP  RF                                                               
*                                                                               
         GOTO1 ADMGR,DMCB,JOUNSET,PRTQID,NDX,WORK,CIREC                         
         CLI   8(R1),0                                                          
         BNE   SELACTX                                                          
*                                                                               
         GOTO1 ADMGR,DMCB,ACTIVATE,PRTQID,NDX,WORK,CIREC                        
         B     SELACTX                                                          
*                                                                               
SELACTN  MVC   0(3,R1),=C'ERR'     ERROR                                        
         B     SELACTX                                                          
SELACTNA MVC   0(3,R1),=C'AUT'     IF NOT OPS YOU CAN'T DO THIS                 
*                                                                               
SELACTX  SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
*************************************************************                   
*        LOADING SCREEN HAS LOST OLD INPUT. GO FIND IT      *                   
*        **NOTE** VERY HARD CODE, CRUDE BUT EFFECTIVE       *                   
*************************************************************                   
REMERGE  NTR1  BASE=*,LABEL=*                                                   
         L     R6,SRPATWA          R6=TWA                                       
         L     R1,SRPAUTL                                                       
         TM    TSTAT8-UTLD(R1),TST8STSS                                         
         BO    EXIT                CAN'T MERGE SOFT FIELD                       
*                                                                               
         L     R1,TBUFF-UTLD(R1)   R1=TBUFF                                     
*                                                                               
         LA    R6,64(R6)           FIRST FIELD                                  
         LA    R1,10(R1)           FIRST FIELD                                  
*                                                                               
REM010   TM    1(R6),X'20'         SKIP PROTECTEDS                              
         BO    REM090                                                           
         LA    RF,8(R6)                                                         
*                                                                               
REM020   CLI   0(R1),X'11'         CHECK FOR SBA                                
         BE    REM050                                                           
         CLI   0(R1),X'03'         TEST FOR END                                 
         BE    EXIT                                                             
         MVC   0(1,RF),0(R1)       MERGE INPUT                                  
         OI    0(RF),X'40'         FORCE UPPER CASE                             
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         B     REM020                                                           
*                                                                               
REM050   LA    R1,3(R1)            NEXT FIELD                                   
         CLI   0(R1),X'03'                                                      
         BE    EXIT                EXIT AFTER LAST                              
*                                                                               
REM090   SR    R0,R0               NEXT FIELD                                   
         ICM   R0,1,0(R6)                                                       
         BZ    EXIT                                                             
         AR    R6,R0                                                            
         B     REM010                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY STATS SCREEN                                     *         
***********************************************************************         
STATS    NTR1  BASE=*,LABEL=*                                                   
         MVC   RUNHED,SPACES                                                    
         MVC   RUNHEDU,SPACES                                                   
         MVC   RUNHED,STATL1                                                    
         MVC   RUNHEDU,STATL2                                                   
*                                                                               
         TIME  BIN                                                              
         ST    R0,TIMENOW                                                       
*                                                                               
         LA    R3,RUNLINH                                                       
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTDSOON)                                            
         MVI   DMCB,X'20'                                                       
         GOTO1 ALOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                                                            
         ICM   R2,7,61(RF)         A(MONS)                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R2,AMONS                                                         
         ICM   R1,15,12(RF)        A(END)                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R1,AEND                                                          
         LAM   AR2,AR2,TBLET         KEEP TAKING THE TBLETS                     
         SAC   512                                                              
         USING MONSOOND,R2                                                      
*                                                                               
         CLI   PFKEY,7             PF7 BACK TO TOP                              
         BE    STATS005                                                         
*                                                                               
STATS001 CLI   PFKEY,8             START FROM BOTTOM                            
         BNE   *+12                                                             
         L     R2,RUNSSBOT                                                      
         B     STATS005                                                         
*                                                                               
         L     R2,RUNSSTOP         ENTER MEANS NO SCROLL                        
         C     R2,AMONS                                                         
         BNL   STATS005                                                         
         L     R2,AMONS                                                         
*                                                                               
STATS005 ST    R2,RUNSSTOP         SAVE TOP ADDR                                
*                                                                               
STATS010 ST    R3,FULL             SAVE ADDRESS OF SCREEN LINE                  
         OC    0(4,R2),0(R2)       TEST EMPTY SLOT                              
         BZ    STATS100            <--WAS NOP                                   
         BZ    STATS016                                                         
         USING STATLIND,R3                                                      
         MVC   STATLINE,SPACES                                                  
*                                                                               
         L     RF,MONSTIME         MONS START TIME                              
         A     RF,DDSTIME                                                       
         BRAS  RE,SHOWTIME                                                      
         MVC   STATTIME,WORK2                                                   
*                                                                               
         MVC   STATNAME,MONSNAME   NAME OF TASK                                 
*                                                                               
         MVC   STATCPU,MONSCPU     CPU ID                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,MONSASID,STATASID+1,2                               
*                                                                               
         MVC   STATJOB,MONSJNUM    A000123                                      
*                                                                               
         LH    R4,MONSASID         GET THE ASID                                 
         LOCASCB ASID=(R4)                                                      
         LR    R5,R1                                                            
         LTR   RF,RF                                                            
*NOP     BNZ   STATS020            ERROR FROM MACRO                             
         BNZ   STATS016            ERROR FROM MACRO                             
*                                                                               
         USING ASCB,R5                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
*NOP     BNE   STATS020            NOT AN ASCB                                  
         BNE   STATS016            NOT AN ASCB                                  
         LM    R0,R1,ASCBEJST                                                   
         SRDL  R0,12               CONVERT TO MICROSEC IN R1                    
         D     R0,=F'10000'                                                     
         LR    RF,R1                                                            
         LM    R0,R1,ASCBSRBT                                                   
         SRDL  R0,12               CONVERT TO MICROSEC IN R1                    
         D     R0,=F'10000'                                                     
         AR    R1,RF               ADD CPU AND SRB TIME                         
         EDIT  (R1),(8,STATCPUT),2                                              
         ST    R1,FULL             SAVE CPU TOTAL                               
*                                                                               
         MVC   STATEXCP,=C'    ....'                                            
*                                                                               
         ICM   R1,15,ASCBJBNS      TASKNAME                                     
         BZ    *+10                                                             
         MVC   STATTASK,0(R1)                                                   
         MVC   STATTASK,MONSPROG                                                
*                                                                               
         CLI   MONSACT,C'I'        LAST EVENT                                   
         BNE   *+10                                                             
         MVC   STATWHAT,=C'INIT'                                                
         CLI   MONSACT,C'R'                                                     
         BNE   *+10                                                             
         MVC   STATWHAT,=C'RUN '                                                
         CLI   MONSACT,C'W'                                                     
         BNE   *+10                                                             
         MVC   STATWHAT,=C'WAIT'                                                
         CLI   MONSACT,C'D'                                                     
         BNE   *+10                                                             
         MVC   STATWHAT,=C'DEAD'                                                
*                                                                               
         L     RF,TIMENOW                                                       
         S     RF,MONSLAST                                                      
*        A     RF,DDSTIME                                                       
         BRAS  RE,SHOWTIME                                                      
         MVC   STATWHEN,WORK2                                                   
*                                                                               
         TM    JOBFLAG,JOBFRUN                                                  
         BZ    STATS011                                                         
         CLI   MONSACT,C'R'                                                     
         BNE   STATS016                                                         
*                                                                               
         SAM31                                                                  
*                                                                               
STATS011 L     R4,ASCBASSB                                                      
         SAM31                                                                  
         ICM   R4,15,ASSBJSAB-ASSB(R4) R4 = A(JSAB)                             
         BZ    STATS016            NO JSAB                                      
*                                                                               
         OC    MONSJOBS,MONSJOBS   ANY JOB INFO IN THIS TASK                    
         BZ    STATS016                                                         
*                                                                               
         LLC   R0,0(R3)                                                         
         AR    R3,R0               NEXT LINE FOR JOB INFO                       
         MVC   STATLINE,SPACES                                                  
         OI    STATHDR+1,X'08'     HIGHT INTENSITY                              
*                                                                               
         USING JSAB,R4                                                          
         MVC   STATJOB,JSABJBID    GET JOBID (E.G., JOB12345)                   
*                                                                               
         L     RF,MONSSTIM         TIME OF ATTACH                               
         A     RF,DDSTIME                                                       
         BRAS  RE,SHOWTIME                                                      
         MVC   STATTIME,WORK2                                                   
*                                                                               
         MVC   STATTASK,MONSPROG   ATTACHED PROGRAM                             
         MVC   STATTASK,MONSMVSJ   ATTACHED PROGRAM                             
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(8),MONSUSER    USERID                                       
         MVC   WORK+9(3),MONSREP   REPORT                                       
         MVC   WORK+13(5),MONSRNUM NUMBER                                       
         GOTO1 VSQUASH,DMCB,WORK,(C',',18)                                      
         LARL  RF,LOWER                                                         
         TR    WORK(18),0(RF)                                                   
         MVC   STATNAME(18),WORK   userid,report,number                         
*                                                                               
         L     R1,FULL             CPU THIS JOB                                 
         S     R1,MONSCPUT                                                      
         EDIT  (R1),(8,STATCPUT),2                                              
*                                                                               
         ICM   RF,15,ASCBOUXB      DIG OUT EXCP COUNT                           
         BZ    STATS01A                                                         
*                                                                               
         LTG   GRF,OUXBIOCA-OUXB(RF) LOAD&TEST GRAND: OUXBIOCA 64BITS           
         BNZ   *+14                                                             
         MVC   STATEXCP,=C'    ....' DOTS IF ALL ZEROS                          
         B     STATS01A                                                         
*                                                                               
* MAX NUMBER THAT WILL FIT IN 8 CHARS IS 99,999,999                             
         CGFI  GRF,99999999        COMPARE GRAND FULLWORD IMMEDIATE             
         BNH   *+14                                                             
         MVC   STATEXCP,=C'    ****' NUMBER TOO BIG                             
         B     STATS01A                                                         
*                                                                               
         EDIT  (RF),(8,STATEXCP)                                                
*                                                                               
STATS01A MVC   STATWHAT(8),=C'Pop  in+'                                         
         L     RF,MONSPTIM                                                      
         A     RF,MONSPOPT                                                      
         C     RF,TIMENOW                                                       
         BNL   STATS012                                                         
*                                                                               
         MVC   STATWHAT(8),=C'Pop  no-'                                         
         L     RE,TIMENOW          SWAP TIME VALUES                             
         ST    RF,TIMENOW                                                       
         LR    RF,RE                                                            
*                                                                               
STATS012 S     RF,TIMENOW                                                       
         BRAS  RE,SHOWTIME                                                      
         MVC   STATWHEN+3(5),WORK2+3  JUST MM:SS                                
         B     STATS020                                                         
*                                                                               
STATS015 CLI   MONSACT,C'D'        FLAGGED AS DEAD AND STILL NO JOB?            
         BNE   *+10                                                             
         XC    MONSTASK,MONSTASK   CLEAN UP THIS TIME ROUND                     
*                                                                               
         CLI   MONSACT,C'R'        IF RUNNING AND NO JOB INFO                   
         BNE   STATS020                                                         
         MVC   STATWHAT,=C'DEAD'   ASSUME DEAD                                  
         MVI   MONSACT,C'D'                                                     
         B     STATS020                                                         
*                                                                               
STATS016 DS    0H                                                               
         LLC   R0,0(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
STATS020 DS    0H                                                               
         LLC   R0,0(R3)                                                         
         AR    R3,R0               NEXT SCREEN LINE                             
         LA    R1,RUNXXXH                                                       
         SHI   R1,L'RUNLINH+L'RUNLIN LESS ONE LINE FROM END                     
         CR    R3,R1                                                            
         BNL   STATS990                                                         
*                                                                               
STATS100 LA    R2,MONSNLGQ(R2)     NEXT ENTRY                                   
         C     R2,AEND                                                          
         BNL   STATS990                                                         
         ST    R2,RUNSSBOT         SAVE BOTTOM                                  
         B     STATS010                                                         
*                                                                               
STATS990 SAC   0                                                                
         CR    RB,RB                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES AND COMMON STORAGE                                  *         
***********************************************************************         
COMMON   DS    0D                                                               
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
EXITOK   CR    RC,RC                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XMOD     CLI   JOBLOCK,YES         NEED TO FREE JOB TABLE?                      
         BNE   *+8                 NO                                           
         BRAS  RE,FREEJOB                                                       
         CLI   CLSLOCK,YES         NEED TO FREE CLASS TABLE?                    
         BNE   *+8                 NO                                           
         BRAS  RE,FREECLS                                                       
*                                                                               
XMOD1    ICM   R1,15,CURSOR                                                     
         BNZ   *+8                                                              
         LA    R1,RUNLINH                                                       
         OI    FHOI-FHD(R1),FHOICU                                              
         L     RD,SAVERD                                                        
*                                                                               
XMODX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL GETHELP AND EXIT TO MONITOR                                    *         
***********************************************************************         
HELPOUT  L     R1,AHELP                                                         
         OI    6(R1),X'40'         SET CURSOR                                   
         LR    R0,R1                                                            
         MVC   HELPKEY,HELPID                                                   
*                                                                               
         CLI   DDS,YES             DDS GET PAGE 2                               
         BNE   *+8                                                              
         MVI   HELPPAG,2                                                        
*                                                                               
         LA    R1,RUNACTH          ACTION HELP                                  
         CR    R1,R0                                                            
         BNE   *+12                                                             
         LA    RF,1                                                             
         B     HELP020                                                          
*                                                                               
         LA    R1,RUNOPTH          OPTION HELP                                  
         CR    R1,R0                                                            
         BNE   HELP010                                                          
         LA    RF,2                                                             
         CLI   ACT.ACTACT,ACTTLIS  TEST LIST                                    
         BE    HELP020                                                          
         LA    RF,3                                                             
         B     HELP020                                                          
*                                                                               
HELP010  LA    RF,4                ELSE SUB HELP                                
*                                                                               
HELP020  STC   RF,HELPNUM                                                       
         XC    DMCB(24),DMCB                                                    
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0)                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ON ERROR FLAG JOB TABLE FOR REBUILD THEN DIE WITH R0 = ERROR        *         
***********************************************************************         
REBUILD  NTR1  ,                                                                
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTFRST                                                
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         MVC   TBJTIME,EFFS        FLAG EMERGENCY REBUILD                       
*                                                                               
         CLI   JOBLOCK,YES         NEED TO FREE JOB TABLE?                      
         BNE   *+8                 NO                                           
         BRAS  RE,FREEJOB                                                       
         CLI   CLSLOCK,YES         NEED TO FREE CLASS TABLE?                    
         BNE   *+8                 NO                                           
         BRAS  RE,FREECLS                                                       
*                                                                               
         DC    H'0'                ABEND - CHECK R0 FOR REASON                  
         DC    CL8'REBUILD'                                                     
         EJECT                                                                  
***********************************************************************         
* ERROR HANDLING                                                      *         
***********************************************************************         
EIIF     LHI   R0,2                INVALID INPUT FIELD                          
         B     ERROR                                                            
EFTS     LHI   R0,59               FIELD TOO SHORT                              
         B     ERROR                                                            
EFTL     LHI   R0,60               FIELD TOO LONG                               
         B     ERROR                                                            
EIAC     LHI   R0,21               INVALID ACTION                               
         B     ERROR                                                            
EFNN     LHI   R0,3                FIELD NOT NUMERIC                            
         B     ERROR                                                            
EIID     LHI   R0,73               INVALID (USER) ID                            
         B     ERROR                                                            
EIKW     LHI   R0,24               INVALID KEYWORD                              
         B     ERROR                                                            
EKWI     LHI   R0,61               KEYWORD INCOMPATIBLE                         
         B     ERROR                                                            
EDKO     LHI   R0,62               DUPLICATED KEYWORD OPTION                    
         B     ERROR                                                            
EIDV     LHI   R0,54               INVALID DATA VALUE                           
         B     ERROR                                                            
EROM     LHI   R0,63               REQUIRED OPTION MISSING                      
         B     ERROR                                                            
EDTS     LHI   R0,65               DATA TOO SHORT                               
         B     ERROR                                                            
EDTL     LHI   R0,64               DATA TOO LONG                                
         B     ERROR                                                            
ENTC     LHI   R0,252              NOTHING TO CHANGE                            
         B     ERROR                                                            
EKEY     LHI   R0,12               INVALID KEYWORD                              
         B     ERROR                                                            
ENTD     LHI   R0,66               NOTHING TO DISPLAY                           
         B     ERROR                                                            
EILA     LHI   R0,57               INVALID LINE ACTION                          
         B     ERROR                                                            
EITN     LHI   R0,115              INVALID TERMINAL NUMBER                      
         B     ERROR                                                            
EMREP    LHI   R0,77               MISSING USER/SUBID/REP#                      
         B     ERROR                                                            
ENTP     LHI   R0,78               NOTHING TO PURGE                             
         B     ERROR                                                            
ECNP     LHI   R0,79               CANNOT PURGE THIS JOB                        
         B     ERROR                                                            
EIAU     LHI   R0,105              NOT AUTHORISED                               
         B     ERROR                                                            
EDISK    LHI   R0,186              DISK ERROR ON PQ                             
         B     ERROR                                                            
EPUR     LHI   R0,79               CANNOT PURGE THIS JOB                        
         B     ERROR                                                            
IJOB     LHI   R0,250              JOB NUMBERS &1 TO &2 OF &3 DISPLAYED         
         B     INFO                                                             
IPRI     LHI   R0,252              QUEUE CHANGED                                
         B     INFO                                                             
IPUR     LHI   R0,11               JOB PURGED                                   
         B     INFO                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT AN ERROR MESSAGE & EXIT                                      *         
***********************************************************************         
ERROR    L     RD,SAVERD                                                        
         XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM ERR NUM                
         GOTO1 VGETTXT,DMCB,(R0),0,(C'E',0),0                                   
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT AN INFO MESSAGE & EXIT                                       *         
***********************************************************************         
INFO     L     RD,SAVERD                                                        
         XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM INF NUM                
         GOTO1 VGETTXT,DMCB,(R0),0,(C'I',0),0,WORK                              
         B     DONE                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE BACK SPECIAL S/R SAVE PAGE (IF READ) & IF ANY JOBTAB ENTRIES  *         
* WERE DELETED BY TRANSACTION COMPRESS SCHEDULER JOB TABLE            *         
***********************************************************************         
DONE     CLI   TWAREAD,YES         TEST TWA WAS READ                            
         BNE   XMOD                NO                                           
*                                                                               
         MVC   RUNSTNUM,TRANSNUM   WRITE BACK S/R SAVE PAGE                     
         MVC   RUNSLO,SEQLO        PUT IN S/R SAVED STORAGE                     
         MVC   RUNSHI,SEQHI                                                     
         MVC   RUNSNUM,SEQNUM                                                   
         MVC   RUNSACT,ACT.ACTACT  SAVE THIS ACTION                             
         GOTO1 ADMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRPATIA                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
* FOR ACTION 'RESET' DON'T EXIT, INSTEAD FORCE 'DISPLAY'                        
         CLI   ACT.ACTACT,ACTTRST                                               
         BNE   XMOD                                                             
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         LTORG                                                                  
RUNSMAXN EQU   (RUNXXXH-RUNLINH)/(RUNHEDUH-RUNHEDH)                             
ARZERO   DC    16F'0'                                                           
*                                                                               
HELPID   DC    XL10'0158FF00000000000000'  SYS/PRG/SCRN                         
*                                                                               
* STATUS SCREEN HEADERS MUST BE 78 CHARACTERS LONG                              
*                ----+----1----+----2----+----3----+----4----+----5             
STATL1   DC    C'Time     Name/Id  Cpu  ASID Number   '                         
         DC    C'CPU Time Excp     Task/Pgm Stat Time     '                     
STATL2   DC    C'-------- -------- ---  ---- -------- '                         
         DC    C'-------- -------- -------- ---- -------- '                     
*                                                                               
LISTL1   DC    C'Act Submit   Cl Fac  P Job Name Ty MonASID '                   
         DC    C'Trm Luid User-ID  Report    Status'                            
LISTL2   DC    C'--- -------- -- ---- - -------- -- ------- '                   
         DC    C'-------- -------- --------- ------'                            
*                                                                               
QUEUEL1  DC    C'             20   40   60   80   100  120  '                   
         DC    C'140  160  180  200  240  260  280 +'                           
QUEUEL2  DC    C'-------- ----|----|----|----|----|----|----'                   
         DC    C'|----|----|----|----|----|----|----'                           
*                                                                               
SPACES   DC    CL80' '                                                          
STARS    DC    60C'*'                                                           
DASHES   DC    60C'-'                                                           
*                                                                               
PUT      DC    C'PUT'                                                           
ADR      DC    C'ADR'                                                           
END      DC    C'END'                                                           
EOJ      DC    C'EOJ'                                                           
*                                                                               
DMREAD   DC    C'DMREAD  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMUNLK   DC    C'DMUNLK  '                                                      
INDEX    DC    C'INDEX   '                                                      
JOUNSET  DC    C'JOUNSET '                                                      
ACTIVATE DC    C'ACT     '                                                      
READ     DC    C'READ    '                                                      
BUFFER   DC    C'BUFFER  '                                                      
PURGE    DC    C'PURGE   '                                                      
HOLD     DC    C'HOLD    '                                                      
ACTV     DC    C'ACTV    '                                                      
SELECT   DC    C'SELECT  '                                                      
*                                                                               
DC@SUMUK DC    CL8'SUMUK'                                                       
DC@RESET DC    CL8'RESET'                                                       
DC@ADV   DC    CL8'ADV'                                                         
DC@MONS  DC    CL8'MONSOON'                                                     
DC@QUEUE DC    CL8'QUEUE  '                                                     
DC@PURGE DC    CL8'PURGE  '                                                     
DC@JCL   DC    CL8'JCL    '                                                     
DC@LONGW DC    CL8'LONGWAIT'                                                    
DC@UPD   DC    CL8'UPDATE'                                                      
DC@COMSC DC    CL8'COMSCORE'                                                    
*                                                                               
TEMPSTR  DC    CL8'TEMPSTR '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
PRTQUE   DC    CL8'PRTQUE  '                                                    
EFFS     DC    16X'FF'                                                          
*                                                                               
DISMSG   DC    C'Job numbers AAA to BBB of CCC displayed'                       
NXTMSG   DC    C' - enter next request'                                         
BIGMSG   DC    C' - Total jobs = nnnn '                                         
*                                                                               
SUMMSG   DC    C'Waiting=####'                                                  
SUMMSG0  DC    C',Running=####'                                                 
SUMMSG1  DC    C',Ave wait=00:00'                                               
SUMMSG2  DC    C' Type B waiting=####'                                          
SUMMSG3  DC    C',00:00'                                                        
SUMMSG4  DC    C',Classes=###'                                                  
*                                                                               
*&&US                                                                           
DDSTIME  DC    A(360000*6)         CORRECTION FACTOR FOR DDS TIME               
*&&                                                                             
*&&UK                                                                           
DDSTIME  DC    A(0)                NO CORRECTION FOR UK                         
*&&                                                                             
*                                                                               
OPERMSG  DS    0X                                                               
OPERID   DS    CL10                                                             
         DS    CL1                                                              
OPERNAME DS    CL8                                                              
         DS    CL1                                                              
OPERT1   DS    CL4                                                              
OPERPRI  DS    CL1                                                              
         DS    CL1                                                              
OPERT2   DS    CL2                                                              
OPERUSER DS    CL8                                                              
         DS    CL9                                                              
OPERMSGL EQU   *-OPERMSG                                                        
         ORG   OPERNAME                                                         
OPERCLR  DS    CL64                                                             
         ORG                                                                    
OPERMSGL2 EQU   *-OPERMSG                                                       
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         DS    0D                                                               
ACTTAB   DC    Y(UC@DSP-WORKD,LC@DSP-WORKD)                                     
         DC    AL1(ACTTDIS,ACTUALL,ACTFDIS,0)                                   
ACTSUM   DC    Y(UC@SUM-WORKD,LC@SUM-WORKD)                                     
         DC    AL1(ACTTSUM,ACTUDDS,ACTFDIS,0)                                   
         DC    Y(UC@SUMUK-WORKD,UC@SUMUK-WORKD)                                 
         DC    AL1(ACTTSUK,ACTUDDS,ACTFDIS,0)                                   
         DC    Y(UC@RESET-WORKD,UC@RESET-WORKD)                                 
         DC    AL1(ACTTRST,ACTUDDS,ACTFDIS,0)                                   
         DC    Y(UC@CLR-WORKD,LC@CLR-WORKD)                                     
         DC    AL1(ACTTCLR,ACTUDDS,0,0)                                         
         DC    Y(UC@STAT-WORKD,LC@STAT-WORKD)                                   
         DC    AL1(ACTTSTA,ACTUDDS,ACTFDIS,0)                                   
         DC    Y(UC@QUEUE-WORKD,LC@QUEUE-WORKD)                                 
         DC    AL1(ACTTQUE,ACTUDDS,ACTFDIS,0)                                   
         DC    Y(UC@PURGE-WORKD,LC@PURGE-WORKD)                                 
         DC    AL1(ACTTPUR,ACTUDDS,0,0)                                         
         DC    Y(UC@PRI-WORKD,LC@PRI-WORKD)                                     
         DC    AL1(ACTTPRI,ACTUDDS,0,0)                                         
ACTLIS   DC    Y(UC@LIST-WORKD,LC@LIST-WORKD)                                   
         DC    AL1(ACTTLIS,ACTUDDS,ACTFDIS,0)                                   
         DC    X'FF'                                                            
*                                                                               
         ORG   ACTTAB                                                           
ACTDIS   DS    0X                                                               
         ORG                                                                    
*                                                                               
*                                                                               
ACTTABD  DSECT                                                                  
ACTUNAM  DS    Y                   DISP TO UPPERCASE NAME                       
ACTLNAM  DS    Y                   DISP TO LOWERCASE NAME                       
*                                                                               
ACTACT   DS    X                                                                
ACTTDIS  EQU   1                   DISPLAY JOB QUEUE                            
ACTTCLR  EQU   2                   CLEAR JOB QUEUE                              
ACTTSUM  EQU   3                   DISPLAY SUMMARY SCREEN                       
ACTTSTA  EQU   4                   DISPLAY STATS SCREEN                         
ACTTQUE  EQU   5                   DISPLAY QUEUE SCREEN                         
ACTTPUR  EQU   6                   PURGE A JOB                                  
ACTTPRI  EQU   7                   SET NEW JOB PRIORITY                         
ACTTLIS  EQU   8                   VIEW REPORT LIST/SEL                         
ACTTSUK  EQU   9                   UK SUMMARY                                   
ACTTRST  EQU   10                  RESET                                        
ACTTPURG EQU   11                  PURGE FORCE                                  
ACTTIGN  EQU   12                  SET TO IGNORE WITH ENQJOBNAME                
*                                                                               
ACTUSER  DS    X                                                                
ACTUALL  EQU   0                   ACTION CAN BE DONE BY EVERYONE               
ACTUDDS  EQU   1                   ACTION IS DDS ONLY                           
*                                                                               
ACTFLAG  DS    XL1                                                              
ACTFDIS  EQU   X'80'               ACTION IS A DISPLAY FUNCTION                 
         DS    XL1                                                              
ACTTABL  EQU   *-ACTTABD                                                        
                                                                                
RUN      CSECT                                                                  
*                                                                               
         DS    0D                                                               
OPTTAB   DS    0XL8                                                             
         DC    X'41F0',S(UC@STAT),A(VALSTAT)                                    
         DC    X'41F0',S(UC@TRM),A(VALTRM)                                      
         DC    X'41F0',S(UC@JOB),A(VALJOB)                                      
         DC    X'41F0',S(UC@SYS),A(VALSYS)                                      
         DC    X'41F0',S(UC@TYPE),A(VALTYP)                                     
         DC    X'41F0',S(UC@CLASS),A(VALCLS)                                    
         DC    X'41F0',S(UC@USRID),A(VALUSER)                                   
         DC    X'41F0',S(UC@RPT),A(VALSUBID)                                    
         DC    X'41F0',S(DC@ADV),A(VALADV)                                      
         DC    X'41F0',S(DC@MONS),A(VALMON)                                     
         DC    X'41F0',S(UC@TIME),A(VALTIME)                                    
         DC    X'41F0',S(UC@AGY),A(VALAGY)                                      
         DC    X'41F0',S(UC@RNING),A(VALRUN)                                    
         DC    X'41F0',S(UC@REG),A(VALREG)                                      
         DC    X'41F0',S(DC@UPD),A(VALUPD)                                      
         DC    X'41F0',S(UC@LONG),A(VALLONG)                                    
         DC    X'41F0',S(DC@LONGW),A(VALLONGW)                                  
         DC    X'41F0',S(DC@JCL),A(VALJCL)                                      
         DC    X'41F0',S(DC@COMSC),A(VALCOMS)                                   
         DC    X'0000'                                                          
*                                                                               
SUBTAB   DS    0X                  ** SUB-ACTION TABLE **                       
         DC    C'UP ',AL1(1)                                                    
         DC    C'DOW',AL1(2)                                                    
         DC    C'HOL',AL1(3)                                                    
         DC    C'REL',AL1(4)                                                    
         DC    C'PUR',AL1(5)                                                    
         DC    C'>  ',AL1(6)       ** TEMP TEST UP **                           
         DC    C'<  ',AL1(7)       ** TEMP TEST DOWN **                         
         DC    C'KIL',AL1(8)       KILL THIS JOB                                
         DC    C'JCL',AL1(9)       SHOW JCL                                     
         DC    C'CLR',AL1(10)      CLEAR DATASPACE ENTRY                        
         DC    C'Z  ',AL1(11)      FORCE PURGE                                  
         DC    C'I  ',AL1(12)      IGNORE                                       
         DC    C'GO ',AL1(13)      RUN IT NOW                                   
         DC    C'FIX',AL1(14)      FIX STUCK COMSCORE REPORT                    
         DC    C'XXX',AL1(255)                                                  
         DS    0H                                                               
*                                                                               
STATAB   DS    0XL6                ** PRIMARY STATUS TABLE **                   
         DC    X'41F0',S(UC@RNING),AL1(JOBFRUN),AL1(0)                          
         DC    X'41F0',S(UC@SUBTD),AL1(JOBFSUB),AL1(0)                          
         DC    X'41F0',S(UC@READY),AL1(JOBFAVA),AL1(0)                          
         DC    X'41F0',S(UC@ERROR),AL1(JOBFERR),AL1(0)                          
         DC    X'41F0',S(UC@HOLD),AL1(JOBFHOLD),AL1(0)                          
         DC    X'41F0',S(UC@KILL),AL1(JOBFKILL),AL1(0)                          
         DC    X'41F0',S(UC@PRGED),AL1(255),AL1(0)                              
         DC    X'0000'                                                          
         EJECT                                                                  
***********************************************************************         
* DATA DICTIONARY LIST                                                *         
***********************************************************************         
DCLIST   DS    0C                                                               
         DCDDL SR#ALL,4,L                                                       
         DCDDL SR#CHNG,8,L                                                      
         DCDDL SR#DSP,8,L                                                       
         DCDDL SR#SYS,8,L                                                       
         DCDDL SR#HOLD,9,L                                                      
         DCDDL SR#HELP,8,L                                                      
         DCDDL SR#NTFD,9,L                                                      
         DCDDL SR#READY,9,L                                                     
         DCDDL SR#RNING,9,L                                                     
         DCDDL SR#SCHLD,9,L                                                     
         DCDDL SR#SUBTD,9,L                                                     
         DCDDL SR#STAT,8,L                                                      
         DCDDL SR#TRM,8,L                                                       
         DCDDL SR#TRM,3,L                                                       
         DCDDL SR#DEL,3,L                                                       
         DCDDL SR#TOP,3,L                                                       
         DCDDL SR#CLEAR,8,L                                                     
         DCDDL SR#SEQC,8,L                                                      
         DCDDL SR#TYPE,8,L                                                      
         DCDDL SR#CLASS,8,L                                                     
         DCDDL SR#USRID,8,L                                                     
         DCDDL SR#RPT,8,L                                                       
         DCDDL SR#PRGED,8,L                                                     
         DCDDL SR#ERROR,8,L                                                     
         DCDDL SR#UNKNW,8,L                                                     
         DCDDL SR#SUMDT,8,L                                                     
         DCDDL SR#SUMM,8,L                                                      
         DCDDL SR#QUEUE,8,L                                                     
         DCDDL SR#PURGE,8,L                                                     
         DCDDL SR#TIME,8,L                                                      
         DCDDL SR#PRI,8,L                                                       
         DCDDL SR#LIST,8,L                                                      
         DCDDL SR#JOB,8,L                                                       
         DCDDL SR#KILL,8,L                                                      
         DCDDL GE#AGY,8,L                                                       
         DCDDL GE#LONG,8,L                                                      
         DCDDL GE#REG,8,L                                                       
         DC    X'00'                                                            
         EJECT                                                                  
LOWER    DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040797A7B7C7D7E7F' 70-7F                     
         DC    XL16'40818283848586878889404040404040' 80-8F                     
         DC    XL16'4091929394959697989940404040409F' 90-9F                     
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' B0-BF                     
         DC    XL16'C0818283848586878889404040404040' C0-CF                     
         DC    XL16'D0919293949596979899404040404040' D0-DF                     
         DC    XL16'E040A2A3A4A5A6A7A8A9404040404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                     
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SAVE STORAGE (SR$RUN IN S/R TWA SAVE PAGE)           *         
***********************************************************************         
RUNSAVED DSECT                                                                  
RUNSTNUM DS    XL2                 LAST $RUN TRANSACTION NUMBER                 
RUNSLO   DS    XL2                 LOW SEQUENCE # ON SCREEN                     
RUNSHI   DS    XL2                 HIGH SEQ # ON SCREEN                         
RUNSNUM  DS    XL2                 MAX #                                        
RUNSACT  DS    X                   LAST ACTION                                  
RUNSSTOP DS    A                   CURRENT STAT POINTER TOP                     
RUNSSBOT DS    A                   CURRENT STAT POINTER BOTTOM                  
*                                                                               
RUNSOFFS DS    19XL2               OFFSET TO REAL ENTRIES                       
*                                                                               
RUNSAVEL EQU   *-RUNSAVED          MUST NOT EXCEED 64 BYTES                     
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
PFKEY    DS    X                                                                
DDS      DS    X                                                                
DSKADR   DS    F                                                                
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
AUTH     DS    CL8                                                              
WORK     DS    XL64                                                             
WORK2    DS    XL64                                                             
DSPHD    DS    XL(L'DSPHDR)                                                     
CSPHD    DS    XL(L'DSPHDR)                                                     
ACTNTRY  DS    XL(ACTTABL)                                                      
SDUB     DS    D                                                                
SWORK1   DS    CL64                                                             
TIMENOW  DS    F                                                                
NUMJOBS  DS    F                                                                
SELFULL  DS    CL3                                                              
         DS    C                                                                
*                                                                               
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
         DS    H                                                                
CURSOR   DS    A                   ADDRESS OF CURSOR FIELD                      
RELO     DS    A                   PROGRAM RELOCATION FACTOR                    
SAVERD   DS    A                   LINK BASE                                    
SAVER2   DS    A                   SAVE AREA FOR DSPACE POINTER                 
TBLET    DS    A                   TABS ALET                                    
AJOBTAB  DS    A                   A(JOB SCHEDULER TABLE)                       
AJOBTABX DS    A                   A(END JOB SCHEDULER TABLE)                   
JOBHDRL  DS    H                   L'JOB TABLE HEADER ENTRY                     
JOBTABL  DS    H                   L'JOB SCHEDULER TABLE ENTRY                  
JOBTABX  DS    A                   A(END OF JOB SCHEDULER TABLE)                
AACTTAB  DS    A                   A(ACTION TABLE)                              
AACTDEF  DS    A                   A(ACTION TABLE - DEFAULT ENTRY)              
AOPTTAB  DS    A                   A(OPTION TABLE)                              
ASUBTAB  DS    A                   A(SUB-ACTION TABLE)                          
ASTATAB  DS    A                   A(JOB STATUS TABLE)                          
TWAPAGE  DS    A                   TWA PAGE/TERMINAL NUMBER                     
VSCANNER DS    V                                                                
VTERMVAL DS    V                                                                
VHEXOUT  DS    V                                                                
VGETFACT DS    V                                                                
VDICTATE DS    V                                                                
VGETHELP DS    V                                                                
VGETTXT  DS    V                                                                
VQSORT   DS    V                                                                
VSQUASH  DS    V                                                                
AFID     DS    V                   A(FACID TAB)                                 
ATCB     DS    A                   A(TCB ENTRY)                                 
ABIGBUF  DS    A                   A(TSAR BUFFER)                               
AHELP    DS    V                   A(HELP FIELD)                                
AEND     DS    V                   A(EOT FOR STAT)                              
AMONS    DS    V                   A(SOT FOR STAT)                              
ACLLAST  DS    V                   A(EOT FOR STAT)                              
*                                                                               
MYSSB    DS    A                                                                
ADMGR    DS    A                                                                
ACALLOV  DS    A                                                                
ATICTOC  DS    A                                                                
ALOCKSPC DS    A                                                                
ADMOD000 DS    V                                                                
AWCTYPE  DS    V                                                                
*                                                                               
SRPARMS  DS    0XL32               SERVICE REQUEST PARAMETER LIST               
SRPASYS  DS    A                   A(SYSFACS)                                   
SRPATIA  DS    A                   A(TIA)                                       
SRPAUTL  DS    A                   A(UTL ENTRY)                                 
SRPACOM  DS    A                   A(COMFACS)                                   
SRPASEL  DS    A                   A(SELIST ENTRY)                              
SRPATWA  DS    A                   A(TWA)                                       
SRPAMAP  DS    A                   A(PHASE MAP)                                 
SRPATIOB DS    A                   A(TRANSLATOR I/O BLOCK)                      
*                                                                               
FADR     DS    A                   A(FIELD HEADER)                              
FERN     DS    X                   FIELD ERROR NUMBER                           
FLDISOK  EQU   255                 FIELD IS OK                                  
FLDNOTI  EQU   1                   FIELD NOT INPUT                              
SYSID    DS    X                   MULTIPLE FIELD INDEX                         
SYSIDFL  DS    X                   FACIFL FOR (SYSID) FACPAK ID ENTRY           
FNDX     DS    X                   MULTIPLE FIELD INDEX                         
HELP     DS    X                   FIELD REQUIRING HELP                         
XTRA     DS    CL8                 EXTRA MESSAGE                                
FLDH     DS    XL8                 EXTRACTED TWA FIELD HEADER                   
FLD      DS    CL80                EXTRACTED TWA FIELD                          
*                                                                               
TWAREAD  DS    C                   C'Y'=SPECIAL S/R TWA PAGE READ               
TERMSTAT DS    XL2                 TERMINAL STATUS BYTES                        
TERMLUID DS    XL8                 TERMINAL LUID                                
TRANSNUM DS    H                   CURRENT TERMINAL TRANSACTION NUMBER          
ACTION   DS    X                   ACTION TYPE (FROM ACTTYPE)                   
SUKCHR   DS    X                   SUMMARY OPTION                               
MYLUID   DS    CL8                                                              
MYUSER   DS    XL2                                                              
INHELP   DS    X                                                                
*                                                                               
JOBFILT  DS    X                   JOB FLAG FILTER (AS FOR JOBFLAG)             
JOBFLAG  DS    X                   JOB FLAG BYTE                                
JOBFRUN  EQU   X'80'               JOB IS RUNNING                               
JOBFSUB  EQU   X'40'               JOB SUBMITTED                                
JOBFAVA  EQU   X'20'               JOB READY                                    
JOBFERR  EQU   X'10'               JOB READY IN ERROR                           
JOBFHOLD EQU   X'08'               JOB IS ON HOLD                               
JOBFUPDT EQU   X'04'               JOB IS UPDATIVE                              
JOBFLONG EQU   X'02'               JOB IS LONG                                  
JOBFKILL EQU   X'01'               JOB STATUS KILL                              
*                                                                               
JOBSYS   DS    CL1                 SYSTEM                                       
JOBTYPE  DS    CL3                 SYSTEM/PROG FILTER                           
JOBCLS   DS    CL2                 JOB CLASS FILTER                             
JOBUSER  DS    XL2                 USERID FILTER                                
JOBUSERC DS    CL8                 USERID (CHARACTER)                           
JOBSUBID DS    CL3                 SUB-ID FILTER                                
JOBREPNO DS    XL2                 REPORT # FILTER                              
JOBADV   DS    CL1                 ADV FILTER                                   
JOBMONS  DS    CL7                 MONSOON FILTER                               
JOBPRI   DS    CL1                 PRIORITY                                     
JOBAGY   DS    CL2                 AGENCY                                       
JCLFILT  DS    CL5                 JCL FILTER                                   
*                                                                               
TOTJOB   DS    A                                                                
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
*                                                                               
STARTPF  DS    H                   START FOR PF ACTION                          
DISSD    DS    X                   BUILD EXTENDED SUMMARY (BY AGENCY)           
                                                                                
*                                                                               
*                                                                               
GIUSER   DS    CL2                 WORK AREA FOR GETUSER                        
GIPREV   DS    CL2                                                              
GIULEN   DS    CL1                                                              
GIUSERID DS    CL8                                                              
*                                                                               
GTADV    DS    X                   ADV NUMBER                                   
GTADVN   DS    CL4                 ADV 4CHR SYSTEM NAME                         
GTADVX   DS    CL4                 ADV 3CHR SYSTEM NAME + AOR                   
*                                                                               
SEQS     DS    0XL10                                                            
SEQLO    DS    H                   LOW JOB SEQUENCE NUMBER DISPLAYED            
SEQHI    DS    H                   HIGH JOB SEQUENCE NUMBER DISPLAYED           
SEQNUM   DS    H                   MAXIMUM NUMBER OF JOBS THAT QUALIFY          
SEQDIS   DS    H                   CURRENT DISPLAY SEQUENCE NUMBER              
SEQCHA   DS    H                   CURRENT CHANGE SEQUENCE NUMBER               
*                                                                               
CHANUM   DS    H                   NUMBER OF QUEUE CHANGES MADE                 
CHATYPE  DS    X                   TYPE OF CHANGE MADE                          
CHATDEL  EQU   X'80'               JOBTAB ENTRY DELETED                         
LONGS    DS    X                                                                
UPDS     DS    X                                                                
*                                                                               
JOBLOCK  DS    C                                                                
CLSLOCK  DS    C                                                                
*                                                                               
OPTR     DS    XL2                 REQUIRED FILTERS                             
OPTX     DS    XL2                 INCOMPATIBLE OPTIONS                         
OPTI     DS    XL2                 OPTIONS INPUT SO FAR                         
IADDR    DS    A                   A(OPTION VALIDATION ROUTINE/TABLE)           
OADDR    DS    A                   A(OUTPUT OPTION VALUE)                       
*                                                                               
SCANLIN  DS    X                   N'ENTRIES IN SCANNER BLOCK                   
SCANBLK  DS    6XL32               SCANNER BLOCK                                
*                                                                               
TABCOUNT DS    F                   NUMBER OF ENTRIES IN JOBTAB                  
AFPRTQUE DS    A                   A(FIRST PQ NAME)                             
ACPRTQUE DS    A                   A(CURRENT PQ NAME)                           
DUMMY    DS    X                                                                
PRTQN    DS    CL7                 PRINT QUEUE NAME                             
         DS    CL4                 PQ REC LEN                                   
R        DS    CL210               PQ REC DATA                                  
*                                                                               
UC@SUMUK DS    CL8                                                              
UC@RESET DS    CL8                                                              
*                                                                               
DSLISTU  DS    0D                                                               
UC@ALL   DS    CL4                                                              
UC@CHNG  DS    CL8                                                              
UC@DSP   DS    CL8                                                              
UC@SYS   DS    CL8                                                              
UC@HOLD  DS    CL9                                                              
UC@HELP  DS    CL8                                                              
UC@NTFD  DS    CL9                                                              
UC@READY DS    CL9                                                              
UC@RNING DS    CL9                                                              
UC@SCHLD DS    CL9                                                              
UC@SUBTD DS    CL9                                                              
UC@STAT  DS    CL8                                                              
UC@TRM   DS    CL8                                                              
UC3TRM   DS    CL3                                                              
UC@DEL   DS    CL3                                                              
UC@TOP   DS    CL3                                                              
UC@CLR   DS    CL8                                                              
UC@SEQC  DS    CL8                                                              
UC@TYPE  DS    CL8                                                              
UC@CLASS DS    CL8                                                              
UC@USRID DS    CL8                                                              
UC@RPT   DS    CL8                                                              
UC@PRGED DS    CL8                                                              
UC@ERROR DS    CL8                                                              
UC@UNKNW DS    CL8                                                              
UC@SUMDT DS    CL8                                                              
UC@SUM   DS    CL8                                                              
UC@QUEUE DS    CL8                                                              
UC@PURGE DS    CL8                                                              
UC@TIME  DS    CL8                                                              
UC@PRI   DS    CL8                                                              
UC@LIST  DS    CL8                                                              
UC@JOB   DS    CL8                                                              
UC@KILL  DS    CL8                                                              
UC@AGY   DS    CL8                                                              
UC@LONG  DS    CL8                                                              
UC@REG   DS    CL8                                                              
*                                                                               
DSLISTL  DS    0D                                                               
LC@ALL   DS    CL4                                                              
LC@CHNG  DS    CL8                                                              
LC@DSP   DS    CL8                                                              
LC@SYS   DS    CL8                                                              
LC@HOLD  DS    CL9                                                              
LC@HELP  DS    CL8                                                              
LC@NTFD  DS    CL9                                                              
LC@READY DS    CL9                                                              
LC@RNING DS    CL9                                                              
LC@SCHLD DS    CL9                                                              
LC@SUBTD DS    CL9                                                              
LC@STAT  DS    CL8                                                              
LC@TRM   DS    CL8                                                              
LC3TRM   DS    CL3                                                              
LC@DEL   DS    CL3                                                              
LC@TOP   DS    CL3                                                              
LC@CLR   DS    CL8                                                              
LC@SEQC  DS    CL8                                                              
LC@TYPE  DS    CL8                                                              
LC@CLASS DS    CL8                                                              
LC@USRID DS    CL8                                                              
LC@RPT   DS    CL8                                                              
LC@PRGED DS    CL8                                                              
LC@ERROR DS    CL8                                                              
LC@UNKNW DS    CL8                                                              
LC@SUMDT DS    CL8                                                              
LC@SUM   DS    CL8                                                              
LC@QUEUE DS    CL8                                                              
LC@PURGE DS    CL8                                                              
LC@TIME  DS    CL8                                                              
LC@PRI   DS    CL8                                                              
LC@LIST  DS    CL8                                                              
LC@JOB   DS    CL8                                                              
LC@KILL  DS    CL8                                                              
LC@AGY   DS    CL8                                                              
LC@LONG  DS    CL8                                                              
LC@REG   DS    CL8                                                              
*                                                                               
NUMRUN   DS    H                                                                
NUMSUB   DS    H                                                                
NUMERR   DS    H                                                                
NUMRDY   DS    H                                                                
NUMUNK   DS    H                                                                
TOTRUN   DS    H                                                                
TOTSUB   DS    H                                                                
TOTSUBB  DS    H                                                                
TOTERR   DS    H                                                                
TOTRDY   DS    H                                                                
TOTUNK   DS    H                                                                
CURTIME  DS    F                                                                
TIMECNT  DS    F                                                                
AVETIME  DS    F                                                                
AVETIMEB DS    F                                                                
PRTQID   DS    CL8                                                              
OLDCLASS DS    CL2                                                              
OLDTYPEB DS    CL1                                                              
CURSORD  DS    XL2                                                              
SCRFULL  DS    X                                                                
USETIME  DS    X                                                                
USERUN   DS    X                                                                
YESMORE  DS    X                                                                
CIMAXRPT DS    H                                                                
INDCOUNT DS    H                   COUNT OF INDEX CALLS PER PQ#                 
SVPQINDX DS    XL40                                                             
MYPQINDX DS    XL40                                                             
NDX      DS    XL40                                                             
KEY      DS    XL25                                                             
*                                                                               
CXREC    DS    0C                  DUMMY CXREC                                  
CIREC    DS    0C                                                               
IO       DS    14336C                                                           
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* TABLE DSECTS                                                        *         
***********************************************************************         
CLASTABD DSECT                     CLASS TABLE                                  
CLASCLAS DS    H                   CLASS CODE                                   
CLASAGY  DS    H                                                                
CLASSML  DS    X                   SHORT/MEDIUM/LONG RUNNING JOBS               
         DS    X                                                                
CLASKEYL EQU   *-CLASTABD                                                       
CLASNSUB DS    H                   TOTAL NUMBER SUBMITTED                       
CLASSUBT DS    XL3                                                              
         DS    X                                                                
CLASTSUB DS    H                   TOTAL NUMBER SUB TODAY                       
CLASNRUN DS    H                   TOTAL NUMBER RUNNING                         
CLASRUNT DS    XL3                                                              
         DS    X                                                                
CLASTRUN DS    H                   TOTAL NUMBER RUN TODAY                       
CLASNRDY DS    H                   TOTAL NUMBER READY                           
CLASTRDY DS    H                   TOTAL NUMBER RDY TODAY                       
CLASWAIT DS    XL4                                                              
CLASWTR  DS    XL4                                                              
CLASTABL EQU   *-CLASTABD                                                       
         EJECT                                                                  
**********************************************************************          
* SCREEN DSECTS                                                      *          
**********************************************************************          
RUNTWAD  DSECT                                                                  
         DS    XL64                                                             
       ++INCLUDE SRRUNFFD                                                       
         ORG   RUNLINH                                                          
       ++INCLUDE SRRUNFED                                                       
         ORG   RUNHEDH                                                          
       ++INCLUDE SRRUNFDD                                                       
         EJECT                                                                  
**********************************************************************          
* SUMMARY LINE DSECT                                                 *          
**********************************************************************          
SUMLINED DSECT                                                                  
SUMLLINH DS    XL8                                                              
SUMLLIN  DS    0X                                                               
SUMLCLS  DS    CL2                 CLASS                                        
         DS    CL1                                                              
SUMLSML  DS    CL1                                                              
         DS    CL1                                                              
SUMLSUB  DS    CL3                 #SUBMITTED NOW                               
         DS    CL1                                                              
SUMTSUB  DS    CL5                 #SUBMITTED TODAY (NEW STYLE)                 
         DS    CL1                                                              
SUMSUBT  DS    CL5                 TIME OF OLDEST SUBMITTED                     
         DS    CL1                                                              
SUMWAIT  DS    CL5                 AVERAGE WAIT                                 
         DS    CL1                                                              
SUMLRUN  DS    CL3                 #RUNNING NOW                                 
         DS    CL1                                                              
SUMTRUN  DS    CL5                 #RUNNING TODAY (NEW STYLE)                   
         DS    CL1                                                              
SUMRUNW  DS    CL5                 DURATION OF OLDEST RUNNING                   
         DS    CL1                                                              
SUMRUNT  DS    CL5                 AVERAGE RUN DURATION                         
         DS    CL1                                                              
SUMLRDY  DS    CL3                 #READY TODAY                                 
         DS    CL1                                                              
SUMTRDY  DS    CL5                 #READY NOW                                   
         DS    CL1                                                              
*                                                                               
SUMLERR  DS    CL4                                                              
         DS    CL1                                                              
SUMLUNK  DS    CL4                                                              
         DS    CL1                                                              
SUMLHH   DS    CL2                                                              
SUMLCOL  DS    CL1                                                              
SUMLMM   DS    CL2                                                              
                                                                                
**********************************************************************          
* SUMMARY LINE DSECT                                                 *          
**********************************************************************          
SUKLINED DSECT                                                                  
SUKLCAT  DS    CL8                                                              
         DS    CL2                                                              
SUKLSUB  DS    CL4                                                              
         DS    CL1                                                              
SUKLRNG  DS    CL3                                                              
         DS    CL1                                                              
SUKLRDY  DS    CL3                                                              
         DS    CL1                                                              
SUKLERR  DS    CL3                                                              
         DS    CL1                                                              
SUKLUNK  DS    CL3                                                              
         DS    CL1                                                              
SUKLHH   DS    CL2                                                              
SUKLCOL  DS    CL1                                                              
SUKLMM   DS    CL2                                                              
         EJECT                                                                  
SUKLLEN  EQU   36                                                               
                                                                                
**********************************************************************          
* DDS DISPLAY LINE DSECT                                             *          
**********************************************************************          
RUNLINED DSECT                     ** DISPLAY/CHANGE LINE **                    
RUNLLINH DS    XL8                                                              
RUNLLIN  DS    0CL78               JOB QUEUE DISPLAY                            
RUNLTIME DS    CL8                 HH.MM.SS                                     
         DS    C                                                                
RUNLCLS  DS    CL2                 CC                                           
RUNLAGY  DS    CL2                 AG                                           
         DS    CL1                                                              
RUNLUSER DS    CL8                 USER-ID                                      
         DS    C                                                                
RUNLREPT DS    CL3                 SSS                                          
RUNLRSD  DS    C                                                                
RUNLRSDQ EQU   C','                                                             
RUNLSEQN DS    CL5                 NNNNN                                        
         DS    C                                                                
RUNLTYP  DS    CL2                 REPORT TYPE                                  
         DS    C                                                                
RUNLADV  DS    CL4                 ADV                                          
         DS    CL1                                                              
RUNLPRTY DS    C                   PRIORITY                                     
         DS    C                                                                
         DS    C                                                                
RUNLNAME DS    CL8                 UUUUSSPP                                     
         DS    C                                                                
RUNLPQNO DS    CL1                 PRINT QUEUE NUMBER                           
         DS    C                                                                
RUNLMONS DS    CL7                 MONSOON                                      
         DS    C                                                                
RUNLTSYM DS    CL8                 LLLLCUDV                                     
         DS    CL1                                                              
RUNLSTAT DS    CL3                 JOB STATUS                                   
         DS    CL2                                                              
         EJECT                                                                  
**********************************************************************          
* USER DISPLAY LINE DSECT                                            *          
**********************************************************************          
USRLINED DSECT                     ** DISPLAY/CHANGE USER LINE **               
USRLLINH DS    XL8                                                              
USRLLIN  DS    0CL74               JOB QUEUE DISPLAY                            
         DS    C                                                                
USRLTIME DS    0CL8                HH.MM.SS                                     
USRLHH   DS    CL2                                                              
USRLHMD  DS    C                                                                
USRLHMDQ EQU   C':'                                                             
USRLMM   DS    CL2                                                              
USRLMSD  DS    C                                                                
USRLMSDQ EQU   C':'                                                             
USRLSS   DS    CL2                                                              
         DS    C                                                                
USRLUSER DS    CL8                 USER-ID                                      
         DS    C                                                                
USRLREPT DS    CL3                 SSS                                          
USRLRSD  DS    C                                                                
USRLRSDQ EQU   C','                                                             
USRLSEQN DS    CL5                 NNNNN                                        
         DS    C                                                                
USRLTYPE DS    CL10                REPORT TYPE                                  
         DS    C                                                                
USRLSTAT DS    CL9                 JOB STATUS                                   
         EJECT                                                                  
**********************************************************************          
* UK LIST SEL LINE DSECT                                             *          
**********************************************************************          
LSTLINED DSECT                     ** DISPLAY/LIST SEL LINE **                  
LSTSELH  DS    XL8                                                              
LSTSEL   DS    CL3                                                              
*                                                                               
LSTLINH  DS    XL8                                                              
LSTLIN   DS    0CL74                                                            
*                                                                               
LSTTIME  DS    CL8                 HH.MM.SS                                     
         DS    C                                                                
LSTCLASS DS    CL2                 CLASS                                        
         DS    C                                                                
LSTFAC   DS    CL4                 FACPAK                                       
         DS    C                                                                
LSTPRTY  DS    CL1                 PRIORITY                                     
         DS    C                                                                
LSTJOBN  DS    CL8                 JOBNAME                                      
         DS    C                                                                
LSTTYPE  DS    CL2                 SOON REPORT TYPE                             
         DS    C                                                                
LSTMONS  DS    CL7                 MONSOON                                      
         DS    C                                                                
LSTTERM  DS    CL8                 TERMINAL                                     
         DS    C                                                                
LSTUSER  DS    CL8                 USER-ID                                      
         DS    C                                                                
LSTREPT  DS    CL9                 REPORT ID                                    
LSTRUNOW DS    0C                                                               
LSTIGNOR DS    C                                                                
LSTSTAT  DS    CL7                 STATUS                                       
         EJECT                                                                  
**********************************************************************          
* STATUS LINE DSECT                                                  *          
**********************************************************************          
STATLIND DSECT                                                                  
STATHDR  DS    XL8                                                              
STATLINE DS    0CL(L'RUNLIN)                                                    
STATTIME DS    CL8                                                              
         DS    CL1                                                              
STATNAME DS    CL8                                                              
         DS    CL1                                                              
STATCPU  DS    CL3                                                              
         DS    CL1                                                              
STATASID DS    CL5                                                              
         DS    CL1                                                              
STATJOB  DS    CL8                                                              
         DS    CL1                                                              
STATCPUT DS    CL8                                                              
         DS    CL1                                                              
STATEXCP DS    CL8                                                              
         DS    CL1                                                              
STATTASK DS    CL8                                                              
         DS    CL1                                                              
STATWHAT DS    CL4                                                              
         DS    CL1                                                              
STATWHEN DS    CL8                                                              
STATSPAR DS    (L'STATLINE-(*-STATLINE))C                                       
         EJECT                                                                  
**********************************************************************          
* OTHER DSECTS                                                       *          
**********************************************************************          
         IAZJSAB                                                                
         IHAASCB                                                                
         IHAASSB                                                                
         IHAOUXB                                                                
* SRDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SRDDEQUS                                                       
         PRINT ON                                                               
* GEDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEDDEQUS                                                       
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FATABSJOB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSJOB                                                      
         PRINT ON                                                               
**********************************************************************          
* JOB TABLE REDEFINITION                                             *          
**********************************************************************          
TBJOBTAB DSECT                                                                  
         ORG   TBJNXT                                                           
TBJOFS   DS    XL4                                                              
         ORG                                                                    
         EJECT                                                                  
* FATABDEQU                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* DMPRTQK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SRRUN00   05/12/20'                                      
         END                                                                    
