*          DATA SET SRPRD00    AT LEVEL 010 AS OF 02/14/13                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041591.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*AS WE DISCOVERED THIS IS NO LONGER IN USE SINCE 5/1/02.                        
*IN FACT, THIS PHASE# IS ALREADY REPLACED BY SRCRP00=CRAPPER.                   
*PHASE T16E00A                                                                  
*INCLUDE RECUP                                                                  
T16E00   TITLE 'SRPRD00 ($PRD) - ACC PROD FACILITY'                             
T16E00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$PRD**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC = A(WORKING STORAGE)                      
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         MVC   SRPARMS(8*4),0(R1)  SAVE SERVICE REQUEST PARAMETER LIST          
SRPARMSD USING SRPARMD,SRPARMS                                                  
*                                                                               
         L     R9,SRPARMSD.SRQASYSF                                             
         USING SYSFACD,R9          R9 = A(SYSTEM FACILITIES)                    
*                                                                               
         BAS   RE,INITLIZE         INITIALIZE COMMON VARIABLES                  
*                                                                               
         GOTO1 VGETFACT,DMCB,0                                                  
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         MVC   THESYSID,FASYSID    SAVE THE SYSTEM ID NUMBER                    
         DROP  RE                                                               
*                                                                               
         LR    RF,RB               CHECK THE SYSTEM TYPE                        
         AH    RF,=Y(FACIDTAB-T16E00)                                           
         USING FACITABD,RF                                                      
SRDAR_00 CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                THIS BETTER EXIST ON FACIDTAB                
*                                                                               
         CLC   FACIID,THESYSID                                                  
         BE    *+12                                                             
         LA    RF,L'FACITAB(RF)                                                 
         B     SRDAR_00                                                         
*                                                                               
         BAS   RE,PROCESS          PROCESS THE EDICT FILE                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZES COMMON VARIABLES                                                  
***********************************************************************         
INITLIZE NTR1                                                                   
         MVI   BITFLAG1,0                                                       
*                                                                               
         LR    R7,RC                                                            
         AH    R7,=Y(IOA1-WORKD)                                                
         ST    R7,AIO1                                                          
*                                                                               
         LR    R7,RC                                                            
         AH    R7,=Y(IOA2-WORKD)                                                
         ST    R7,AIO2                                                          
*                                                                               
         LR    R7,RC                                                            
         AH    R7,=Y(SPULAREA-WORKD)                                            
         ST    R7,ASPLAREA                                                      
*                                                                               
         LR    R7,RC               FOR SAVING EDICT RECORDS                     
         AH    R7,=Y(WRKRIOA-WORKD)                                             
         ST    R7,AWRKRIOA                                                      
*                                                                               
         MVC   AWRKRBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         LR    R7,RC               FOR EDICT                                    
         A     R7,=A(HUGEBLCK-WORKD)                                            
         ST    R7,AHUGEBLK                                                      
*                                                                               
         L     R8,VSSB             R8 = A(SSB)                                  
         USING SSBD,R8                                                          
         NI    SSBJFLAG,255-SSBJFWKR STOP ABEND LOOPS                           
         MVC   SYSNAME,SSBSYSNA                                                 
         MVC   SYSN1,SSBSYSN1                                                   
         MVC   RECLEN,SSBTWAL      SAVE TEMPSTR TWA RECORD LENGTH               
         NI    SSBDARFL,X'FF'-SSBDRSDR     DON'T HAVE TASKER CALL AGAIN         
         DROP  R8                                                               
*                                                                               
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         MVC   TERMNUM,TNUM        SAVE TERMINAL NUMBER                         
         MVC   USERNUM,TUSER       SAVE USER ID NUMBER                          
         DROP  R1                                                               
***************                                                                 
* COMFACS STUFF                                                                 
***************                                                                 
         L     R1,SRPARMSD.SRQACOMF    R1 = A(COMFACS)                          
         USING COMFACSD,R1                                                      
         MVC   VADDAY,CADDAY                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VSWITCH,CSWITCH                                                  
         DROP  R1                                                               
         MVC   VRECUP,=V(RECUP)                                                 
         L     R1,RELO                                                          
         L     R0,=A(GOMSPACK)                                                  
         AR    R0,R1                                                            
         ST    R0,VMSPACK                                                       
         L     R0,=A(GOMSUNPK)                                                  
         AR    R0,R1                                                            
         ST    R0,VMSUNPK                                                       
***************                                                                 
* CORERES STUFF                                                                 
***************                                                                 
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4,=X'D9000A0C'    SPOOL                                     
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASPOOL,DMCB                                                      
*                                                                               
***************                                                                 
* GET TODAY'S DATE                                                              
***************                                                                 
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(5,CTODAY)                                 
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
         GOTO1 VDATCON,DMCB,(0,WORK),(5,CTOMORW)                                
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS THE WORKER FILE                                                       
***********************************************************************         
PROCESS  NTR1                                                                   
         BAS   RE,WRKRCREA                                                      
*                                                                               
         L     R1,AIO2              * SCRIPT HEADER *                           
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'ACPRDJOB'                                            
         MVI   18(R1),C'I'                                                      
         MVC   30(5,R1),=C'00004'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00726'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00064'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'                                                      
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000001'                                               
         MVC   10(20,R1),=CL20'                    '                            
         MVC   30(8,R1),MOCFTOID                                                
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   38(3,R1),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         MVC   46(3,R1),=C'GHO'    FOR TEST ID                                  
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'53'        49 + 4 BYTES FOR QSAM                      
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2             * INPUT DATA *                               
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000002'                                               
         MVC   10(20,R1),=CL20' '                                               
         LA    R1,30(R1)                                                        
         USING LAYOUTD,R1                                                       
         MVI   LAYCTRY,LAYCUSA                                                  
         MVI   LAYACT,LAYAAUTO     AUTO                                         
         MVC   LAYCLT,=CL6'ABC'                                                 
         MVC   LAYPRD,=CL6'CAR'                                                 
         MVC   LAYJOB,=CL6'P'                                                   
         MVC   LAYCDATE,CTOMORW                                                 
         MVC   LAYUFLD1,=CL30'0'                                                
         MVC   LAYUFLD2,=CL30'1'                                                
         MVC   LAYUFLD3,=CL30'2'                                                
         MVC   LAYUFLD4,=CL30'FEB28/99'                                         
         LA    R1,LAYEND                                                        
         DROP  R1                                                               
         L     RE,AIO2                                                          
         SR    R1,RE                                                            
         STH   R1,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2             * INPUT DATA *                               
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000003'                                               
         MVC   10(20,R1),=CL20' '                                               
         LA    R1,30(R1)                                                        
         USING LAYOUTD,R1                                                       
         MVI   LAYCTRY,LAYCUSA                                                  
         MVI   LAYACT,LAYAAUTO     AUTO                                         
         MVC   LAYCLT,=CL6'ABC'                                                 
         MVC   LAYPRD,=CL6'CAR'                                                 
         MVC   LAYJOB,=CL6'P'                                                   
         MVC   LAYCDATE,CTOMORW                                                 
         MVC   LAYUFLD1,=CL30'0'                                                
         MVC   LAYUFLD2,=CL30'1'                                                
         MVC   LAYUFLD3,=CL30'2'                                                
         MVC   LAYUFLD4,=CL30'FEB28/99'                                         
         MVC   LAYUFLD5,=CL30'SHOULD BE ERROR'                                  
         LA    R1,LAYEND                                                        
         DROP  R1                                                               
         L     RE,AIO2                                                          
         SR    R1,RE                                                            
         STH   R1,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
         GOTO1 WRKRSEND,DMCB,AIO2                                               
         BAS   RE,WRKRCLOS                                                      
*                                                                               
         B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZES EDICT/DARE STUFF                                                  
***********************************************************************         
DAREINIT NTR1                                                                   
***************                                                                 
* GET ADDRESS OF EDICT FILE                                                     
***************                                                                 
         CLI   WHCHEDCT,C'R'                         REP EDICT FILE?            
         BE    DINIT10                                                          
         GOTO1 VDATAMGR,DMCB,=C'DTFADD',=C'EDCTA'    NO, SOME ADV EDICT         
         B     DINIT20                                                          
*                                                                               
DINIT10  GOTO1 VDATAMGR,DMCB,=C'DTFADD',=C'EDCTR'    YES, REP'S EDICT           
DINIT20  L     RE,DMCB+12          A(DTF)                                       
         TM    36(RE),X'40'        IS THIS FILE NO-OP?                          
         BNZ   DINITNO                                                          
*                                                                               
         MVC   EDICTFL,12(R1)      A(EDICT DCB)                                 
         MVI   EDICTFL,0           CLEAR HOB                                    
***************                                                                 
* FIND NUMBER OF PHYSICAL RECORDS PER BLOCK                                     
***************                                                                 
*&&DO                                                                           
         GOTO1 VDADDS,DMCB,DARPT,AHUGEBLK,F'14336',EDICTFL                      
         MVC   EDCTFRPT,DMCB+10    # OF BLOCKS/TRACK                            
         OC    EDCTFRPT,EDCTFRPT                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         GOTO1 VDADDS,DMCB,RDID,AHUGEBLK,0,EDICTFL,=X'00010100',0               
         OC    8(2,R1),8(R1)                                                    
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         L     R7,AHUGEBLK                                                      
         USING EDFILD,R7                                                        
         CLI   EDFMON,EDFMONPQ                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EDCTFTPD,EDFTKPDY   SAVE NUMBER OF TRACKS PER DAY                
         ZIC   R0,EDFBKPTK         SAVE NUMBER OF RECORDS PER BLOCK             
         STH   R0,EDCTFRPT                                                      
         ZIC   R0,EDFRCPBK         SAVE NUMBER OF RECORDS PER BLOCK             
         STH   R0,EDCTRPBQ                                                      
         MVC   EDCTLRCL,EDFLRECL   SAVE LENGTH OF A LOGICAL RECORD              
         DROP  R7                                                               
*                                                                               
         GOTO1 DAREDATE,DMCB,BTODAY  CALCULATE TRACK INFO FOR TODAY             
         MVC   EDCTFDSK(L'FRSTTRK),FRSTTRK   TRACK NUMBER                       
*                                                                               
DINITYES B     YES                                                              
*                                                                               
DINITNO  B     NO                                                               
         SPACE 2                                                                
***********************************************************************         
* CALCULATES TRACK INFORMATION FOR A SPECIFIED DATE                             
*                                                                               
* ON ENTRY:    PARAM 1             A(YMD 3-BYTE BINARY)                         
***********************************************************************         
DAREDATE NTR1                                                                   
         L     RE,0(R1)            RE=A(DATE)                                   
*                                                                               
         ZIC   R2,2(RE)            DAY NUMBER                                   
         BCTR  R2,0                                                             
         MH    R2,EDCTFTPD                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER FOR DATE          
         STH   R2,FRSTTRK                                                       
         LH    R1,EDCTFTPD                                                      
         BCTR  R1,0                                                             
         AR    R1,R2                                                            
         STH   R1,EDCTFLST         LAST TRACK NUMBER FOR DATE                   
*                                                                               
DDATEX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS INFORMATION FOR DARE FROM THE SSB                           
***********************************************************************         
SETUPSSB NTR1                                                                   
         L     RE,AWRKRIOA         CLEAR THE WORKER IOAREA FIRST                
         LH    RF,=Y(WRECQLNQ)                                                  
         XCEFL                                                                  
         L     RE,AWRKRIOA         CLEAR THE WORKER IOAREA FIRST                
         MVI   1(RE),2             NO ENTRIES                                   
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         CLC   BTODAY,SSBDARDT     GOT SOMETHING FOR TODAY?                     
         BE    SSSB50              YES, READ THE EDICT ADDRESS                  
         DROP  RE                                                               
*                                  NO, SAVED INFO FOR TODAY?                    
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             EXIT PROGRAM, SHOULD WAIT FOR IT             
*                                                                               
         XC    KEY,KEY             SEE IF IT EXISTS FOR THIS SYSTEM             
         LA    R6,KEY                                                           
         USING CTDARKEY,R6                                                      
         MVI   CTDARTYP,CTDARTYQ                                                
         MVI   CTDARSUB,CTDARSBQ                                                
         MVC   CTDARSYS,THESYSID   FOR EACH SYSTEM                              
         DROP  R6                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'88',DMRDHI),CTFILE,KEY,AIO1                     
         CLI   DMCB+8,2            RECORD DELETED?                              
         BE    SSSBNO                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO1             CHECK THE RECORD                             
         USING CTDARREC,R6                                                      
         CLC   CTDARKEY,KEY        FOUND RECORD FOR THIS SYSTEM?                
         BNE   SSSBNO              NO                                           
*                                                                               
         LA    R6,CTDAR1ST                                                      
         MVI   ELCODE,CTDARELQ     LOOK FOR THE EDICT INFO ELEMENT              
         BAS   RE,FIRSTEL                                                       
         BNE   SSSBNO                                                           
*                                                                               
         USING CTDARELD,R6                                                      
         CLC   CTDARDAT,BTODAY     DO WE HAVE PRIOR BUSINESS DAY'S?             
         BNL   SSSB30              NO, MUST BE TODAY                            
*                                                                               
         OI    BITFLAG1,BF1YSDAY   YESTERDAY'S OR SOME PRIOR DAY'S              
         MVC   PRIORDAT,CTDARDAT   SAVE THE DATE OF THAT DAY                    
         GOTO1 DAREDATE,DMCB,PRIORDAT                                           
*                                                                               
SSSB30   L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         LA    R1,SSBDAREA                                                      
         LA    RF,CTDAREDA                                                      
         CLI   WHCHEDCT,C'A'                                                    
         BE    *+12                                                             
         LA    R1,SSBDARER                                                      
         LA    RF,CTDAREDR                                                      
         MVC   0(L'SSBDAREA,R1),0(RF)                                           
         DROP  R6,RE                                                            
*                                                                               
SSSB50   L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         LA    R1,SSBDAREA                                                      
         CLI   WHCHEDCT,C'A'                                                    
         BE    *+8                                                              
         LA    R1,SSBDARER                                                      
*                                                                               
         OC    0(4,R1),0(R1)       NO ADDRESS HERE?                             
         BZ    SSSBNO              NONE, CALCULATE ADDRESS OURSELVES            
*                                                                               
         MVC   EDCTFDSK(3),0(R1)   RESTORE LAST TRACK AND BLOCK READ            
         MVC   RECSKIP,3(R1)       AND WHICH RECORD NUMBER WE'RE UPTO           
         DROP  RE                                                               
*                                                                               
SSSBYES  B     YES                                                              
*                                                                               
SSSBNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* TO FIND NEW STARTING POINT -- GIVEN RECSKIP = # RECORD TO SKIP                
*                               INTO THE BLOCK                                  
***********************************************************************         
RECBUMP  NTR1                                                                   
         ZIC   R1,RECSKIP                                                       
         BCTR  R1,0                                                             
         STC   R1,RECSKIP                                                       
*                                                                               
         LA    R1,1(R1)            ADD BACK THAT 1                              
         CH    R1,EDCTRPBQ         IF LAST REC WAS LAST IN BLOCK                
         BL    RECB05              NO                                           
         MVI   RECSKIP,0           YES, THEN JUST READ NEXT BLOCK               
         SR    R1,R1                                                            
         ZIC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         OI    BITFLAG1,BF1SKIPB   AND DON'T SKIP INTO IT                       
         B     RECB50                                                           
*                                                                               
RECB05   SR    R1,R1                                                            
         B     RECB40                                                           
RECB10   STH   R2,EDCTFDSK         TRACK NUMBER                                 
*                                                                               
         LA    R2,1                                                             
RECB20   STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         BAS   RE,READBLK                                                       
         L     R7,AHUGEBLK         R7-POINTER IN BLOCK                          
         USING EDFILD,R7                                                        
*                                                                               
RECB40   TM    BITFLAG1,BF1SKIPB   SKIP ANY MORE BUMPING?                       
         BO    RECBYES                                                          
         AH    R7,EDCTLRCL         BUMP TO NEXT RECORD                          
         LA    R1,1(R1)                                                         
         ZIC   R2,RECSKIP                                                       
         CR    R1,R2               HAVE WE SKIPPED ENOUGH YET?                  
         BH    RECBYES             YES                                          
         CH    R1,EDCTRPBQ         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   RECB40                                                           
*                                                                               
RECB50   ZIC   R2,EDCTFDSK+2                                                    
         LA    R2,1(R2)            NO                                           
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   RECB20              YES                                          
*                                                                               
         LH    R2,EDCTFDSK                                                      
         LA    R2,1(R2)                                                         
         CH    R2,EDCTFLST                                                      
         BNH   RECB10                                                           
         DC    H'0'                ALL TRACKS FOR TODAY ARE USED                
*                                                                               
RECBYES  LA    R1,1(R1)                                                         
         STC   R1,RECNUM                                                        
*                                                                               
RECBX    XIT1  REGS=(R7)                                                        
         EJECT                                                                  
***********************************************************************         
* CALCULATE NEW DISK ADDRESS FROM SAVED STORAGE                                 
***********************************************************************         
TRKBLK   DS    0H                                                               
         OC    DAFILT,DAFILT                                                    
         BZ    TRKB10                                                           
         MVC   DSKAD,DAFILT                                                     
         XC    DAFILT,DAFILT                                                    
*                                                                               
TRKB10   OC    DSKAD,DSKAD                                                      
         BZ    TRKBX                                                            
         XC    EDCTFDSK,EDCTFDSK                                                
         MVC   EDCTFDSK(3),DSKAD   MOVE IN TRACK AND BLOCK NUMS                 
         MVC   RECSKIP,DSKAD+3     LAST REC NUM                                 
*                                                                               
TRKBX    BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* PUTS THE RECORD IN AIO1 INTO THE WORKER IO AREA                               
*                                                                               
* NOTE: FORMAT OF THE RECORD IN AIO1 IS:                                        
*              BYTE  0             LENGTH OF THE RECORD (SAY N)                 
*              BYTES 1-(N-1)       ACTUAL RECORD                                
***********************************************************************         
PUTINIOA NTR1                                                                   
         L     R6,AWRKRIOA         R6 = A(WHERE TO PUT THE RECORD)              
         OC    0(2,R6),0(R6)       IF WE HAVE AN ERROR                          
         BZ    PIOAX               THEN DON'T ADD TO IOA                        
*                                                                               
         ZICM  RE,0(R6),2                                                       
         AR    R6,RE                                                            
*                                                                               
         L     RE,AIO1             COPY RECORD OVER TO WORKER RECORD            
         ZICM  R1,0(RE),1                                                       
         BZ    PIOAX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
*                                                                               
         LA    RE,1(R1,R6)         RE = A(WHERE TO PUT NEXT RECORD)             
         L     R6,AWRKRIOA                                                      
         SR    RE,R6                                                            
         STCM  RE,3,0(R6)                                                       
         CH    RE,=Y(WRECQLNQ)     MAKE SURE WE DON'T EXCEED THE LENGTH         
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PIOAX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DETERMINES WHICH TYPE OF DARE RECORD WE HAVE AND THEN ROUTES IT TO            
* THE CORRECT PROCESSING ROUTINE                                                
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD)                              
***********************************************************************         
WORKITIN NTR1                                                                   
         USING EDFILD,R7                                                        
WITIN00  LA    R6,EDFDARE          R6 = A(DARE REC WITHOUT EDICT INFO)          
         LA    R1,DAREOBJS                                                      
         USING DAREOBJD,R1                                                      
*                                                                               
WITIN10  CLI   DOBJTID,0           MATCHED ON TRANSMISSION ID?                  
         BE    WITINNO             NO                                           
*                                                                               
WITIN15  CLC   DOBJTID,0(R6)       MATCH ON THIS TRANSMISSION ID?               
         BE    WITIN20                                                          
         LA    R1,DOBJNXT          NO, CHECK NEXT TRANSMISSION ID               
         B     WITIN10                                                          
*                                                                               
WITIN20  MVC   DOBJNUMB,DOBJRIDN   YES, SAVE THE RETURN ID NUMBER               
         DROP  R1                                                               
*                                                                               
         LR    R7,R6               TO FREE R6 FOR GENERAL IO STUFF              
         ZIC   RF,DOBJNUMB                                                      
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     WITINTBL(RF)                                                     
*                                                                               
WITINTBL B     PROCDLNT                                                         
         B     PROCOAPP                                                         
         B     PROCOREJ                                                         
         B     PROCORCM                                                         
         B     PROCOTLR                                                         
         B     PROCOCFM                                                         
         B     PROCOLIN                                                         
         B     PROCERNT                                                         
         B     PROCARCL                                                         
         B     PROCORAK                                                         
         B     PROCMGOK                                                         
         B     PROCMGCN                                                         
         B     PROCDFAX                                                         
         B     PROCCNFX                                                         
*                                                                               
WITINNO  B     NO                                                               
         DROP  R7                                                               
*                                                                               
DAREOBJS DC    CL6'DLNNOT',AL1(DOBJDLNQ)                                        
         DC    CL6'ORDAPP',AL1(DOBJOAPQ)                                        
         DC    CL6'ORDREJ',AL1(DOBJORJQ)                                        
         DC    CL6'ORDCOM',AL1(DOBJOCMQ)                                        
         DC    CL6'ORDTLR',AL1(DOBJOTRQ)                                        
         DC    CL6'ORDCFM',AL1(DOBJOCFQ)                                        
         DC    CL6'ORDLIN',AL1(DOBJOLNQ)                                        
         DC    CL6'ERRNOT',AL1(DOBJERRQ)                                        
         DC    CL6'AGYRCL',AL1(DOBJARCQ)                                        
         DC    CL6'ORDRCL',AL1(DOBJORAQ)                                        
         DC    CL6'MKGROK',AL1(DOBJMOKQ)                                        
         DC    CL6'MKGCAN',AL1(DOBJMCNQ)                                        
         DC    CL6'DLNFAX',AL1(DOBJDFXQ)                                        
         DC    CL6'CANFAX',AL1(DOBJCFXQ)                                        
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE DELIVERY NOTIFICATION FOR AGENCY                                
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCDLNT DS    0H                                                               
         USING RDLNNOTD,R7                                                      
PDLNTD   USING RTN2SNDR,RDNTRTRN                                                
         MVC   QREPCON,RDNTRPCN    COPY THESE VALUES                            
         MVC   QRETURN,RDNTRTRN                                                 
*                                                                               
         CLI   WHCHEDCT,C'R'       REP EDICT IN USE?                            
         BE    PROCDLNR                                                         
*                                                                               
         MVC   AGENCY,PDLNTD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,PDLNTD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED          AGY GOT SPANKED TO DIFF ADV?                 
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   PDLNTNO             YES                                          
*                                                                               
         GOTO1 VHEXIN,DMCB,PDLNTD.RTNAGYMD,BAGYMD,2                             
         DROP  PDLNTD                                                           
*                                                                               
         GOTO1 CALCORDR,DMCB,RDNTORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   PDLNTNO                                                          
*                                                                               
         MVC   USERID,RDNTTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   PDLNTNO                                                          
*                                                                               
         MVC   QMGGROUP,MDNTOFRI-MDLNNOTD(R7)   COULD BE DELNOT FOR MKG         
*                                                                               
         BAS   RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   PDLNTNO                                                          
         CLC   QMGGROUP,=C'  '     DELNOT FOR MKGD?                             
         BH    PDLNT40                                                          
*                                                                               
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PDLNT10  CLI   0(R6),0                                                          
         BNE   PDLNT15                                                          
         GOTO1 =A(SNDERROR),DMCB,REFORDNT,RR=RELO     NEVER TRANSMITTED         
         B     PDLNTNO                                                          
*                                                                               
PDLNT15  CLI   0(R6),DOXMTELQ                                                   
         BE    PDLNT20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDLNT10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
PDLNT20  GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDNTTIME,DUB+L'DOXMTDND,L'RDNTTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    PDLNT30                                                          
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    PDLNT30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    PDLNTNO                    YES, IGNORE THIS RECORD               
         BL    PDLNT30                    NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   PDLNTNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
PDLNT30  MVC   DOXMTDND,DUB                                                     
         MVC   DOXMTDNT,DUB+L'DOXMTDND                                          
         MVC   DOXMTDID,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
         B     PDLNT80                                                          
         DROP  R6                                                               
***************                                                                 
* DELNOT'S FOR AGY REPSONSE TO REP MAKEGOODS                                    
***************                                                                 
PDLNT40  DS    0H                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVI   QMED,C'T'                                                        
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
**       MVC   BCLT,DOIDCLT                                                     
**       GOTO1 VCLUNPK,DMCB,DOIDCLT,QCLT                                        
**       MVC   QPRD1,DOIDPRD                                                    
**       MVC   QPRD2,DOIDPRD2                                                   
**       MVC   BEST,DOIDEST                                                     
**       EDIT  (B1,DOIDEST),(3,QEST1),FILL=0                                    
**       XC    WORK,WORK                                                        
**       MVC   WORK+2(L'DOISTA),DOISTA                                          
**       GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         DROP  R6                                                               
*                                                                               
         BAS   RE,GETNOTCE         READ THE MAKEGOOD NOTICE                     
         BNE   PDLNTNO                                                          
*                                                                               
         XC    ELEM,ELEM           WRITE A ERROR STATUS ELEM TO REC             
         LA    R6,ELEM                                                          
         USING MNSTELD,R6                                                       
         MVI   MNSTEL,MNSTELQ                                                   
         MVI   MNSTLEN,MNSTLENQ                                                 
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,MNSTDATE)                          
         GOTO1 VHEXIN,DMCB,RDNTTIME,MNSTTIME,L'RDNTTIME                         
         MVI   MNSTSTAT,MNSTDELV   DELIVERED STATUS                             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1             LASTEST FIRST                                
         LA    R6,MNRFRST-MNKEY(R6)                                             
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6),RR=RELO                        
*                                                                               
PDLNT80  XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         GOTO1 VDATAMGR,DMCB,PUTREC,SPTFIL,KEY,AIO1,DMWORK                      
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
PDLNTYES BAS   RE,DAREMAIL                                                      
         B     YES                                                              
*                                                                               
PDLNTNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE DELIVERY NOTIFICATION FOR REP                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCDLNR DS    0H                                                               
         MVC   REP,RDNTTOID        GET FROM ID FOR REP IDENTIFICATION           
         BAS   RE,SWTCHCTR         SWITCH CONTROL TO REP  SYSTEM                
         BNE   PDLNRNO             NOT SUCCESSFUL                               
*                                                                               
         CLC   =CL8' ',RDNTRPCN                                                 
         BNL   PDLNRNO                                                          
         CLC   =CL8'0',RDNTRPCN                                                 
         BE    PDLNRNO                                                          
         CLC   =8C'0',RDNTRPCN                                                  
         BE    PDLNRNO                                                          
         CLC   =CL6'UNDARE',RDNTRPCN                                            
         BE    PDLNRNO                                                          
*                                        READ REP CONTRACT INTO AIO1            
         GOTO1 =A(GETRPCON),DMCB,RDNTRPCN,RR=RELO                               
         BNE   PDLNRNO                                                          
*                                                                               
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,RCONELEM-RCONREC(R6)                                          
PDLNR10  CLI   0(R6),0                                                          
         BNE   PDLNR15                                                          
         GOTO1 =A(SNDERROR),DMCB,REFORNTL,RR=RELO    NOT LINKED TO CON.         
         B     PDLNRNO                                                          
*                                                                               
PDLNR15  CLI   0(R6),X'1D'                                                      
         BE    PDLNR20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDLNR10                                                          
*                                                                               
         USING RCONDREL,R6                                                      
*                                                                               
PDLNR20  EQU   *                                                                
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(2,RCONDRDD)                           
*                                  INSERT DATE DELIVERED                        
         GOTO1 VHEXIN,DMCB,RDNTTIME,RCONDRTD,L'RDNTTIME                         
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         L     R6,AIO1             RESET A(IO AREA)                             
         MVC   KEY(L'RCONKEY),0(R6)                                             
*                                  LOAD UP THE KEY AGAIN                        
         GOTO1 VDATAMGR,DMCB,PUTREC,=C'REPFILE',KEY,AIO1,DMWORK                 
*                                  REWRITE THE RECORD                           
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
PDLNRYES B     YES                                                              
*                                                                               
PDLNRNO  B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER APPROVAL                                                  
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOAPP DS    0H                                                               
         USING RORDAPPD,R7                                                      
POAPPD   USING RTN2SNDR,ROAPRTRN                                                
         MVC   AGENCY,POAPPD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,POAPPD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED                                                       
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   POAPPNO             YES, AGENCY DOESN'T BELONG HERE              
*                                                                               
         GOTO1 VHEXIN,DMCB,POAPPD.RTNAGYMD,BAGYMD,2                             
         DROP  POAPPD                                                           
*                                                                               
         MVC   QREPCON,ROAPRPCN    COPY THESE VALUES                            
         MVC   QRETURN,ROAPRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,ROAPORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   POAPPNO                                                          
*                                                                               
         MVC   USERID,ROAPTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   POAPPNO                                                          
*                                                                               
         BAS   RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   POAPPNO                                                          
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   DOIDCON,ROAPRPCN    SAVE THE REP CONTRACT NUMBER                 
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
POAPP10  CLI   0(R6),0                                                          
         BNE   POAPP15                                                          
         GOTO1 =A(SNDERROR),DMCB,REFORDNT,RR=RELO   ORD NOT TRANSMITTED         
         B     POAPPNO                                                          
*                                                                               
POAPP15  CLI   0(R6),DOXMTELQ                                                   
         BE    POAPP20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     POAPP10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
POAPP20  GOTO1 VDATCON,DMCB,(0,ROAPDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROAPTIME,DUB+L'DOXMTSTD,L'ROAPTIME                   
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    POAPP40                                                          
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    POAPP30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTSTD,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    POAPPNO                    YES, IGNORE THIS RECORD               
         BL    POAPP30                    NO, OLDER                             
         CLC   DOXMTSTT,DUB+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?           
         BH    POAPPNO                    YES, IGNORE THIS REC                  
*                                                                               
POAPP30  CLI   DOXMTSTA,QAPP       CAN'T CHANGE ANYTHING BUT APPROVED           
         BE    POAPP40                STATUS                                    
*                                                                               
         CLI   DOXMTSTA,QRECALL    IF RECALL STATUSES THEN DON'T MARK           
         BL    POAPP35                RECORD AS APPROVED                        
         CLI   DOXMTSTA,QRCLUNKN                                                
         BNH   POAPPNO                                                          
         CLI   DOXMTSTA,QRCLTRNS                                                
         BE    POAPPNO                                                          
*                                                                               
POAPP35  GOTO1 =A(SNDERROR),DMCB,REFCCHNG,RR=RELO                               
         B     POAPPNO                                                          
*                                                                               
POAPP40  MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTSTD                                          
         MVI   DOXMTSTA,QAPP       APPROVED                                     
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         GOTO1 VDATAMGR,DMCB,PUTREC,SPTFIL,KEY,AIO1,DMWORK                      
*                                                                               
         GOTO1 =A(SEEDCONN),DMCB,(RC),ROAPRPCN,RR=RELO                          
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
POAPPYES BAS   RE,DAREMAIL                                                      
         B     YES                                                              
*                                                                               
POAPPNO  B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER REJECTION                                                 
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOREJ DS    0H                                                               
         USING RORDREJD,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORJRTRN+L'RORJRTRN-RORDREJD+1                             
         MVC   1(RORJRTRN+L'RORJRTRN-RORDREJD,R6),0(R7)                         
         BAS   RE,PUTINIOA                                                      
*                                                                               
         OI    BITFLAG1,BF1DPEND   GOT A PENDING DARE RECORD                    
*                                                                               
POREJX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER REJECTION COMMENT                                         
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCORCM DS    0H                                                               
         USING RORDCOMD,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),ROCMTEXT+L'ROCMTEXT-RORDCOMD+1                             
         MVC   1(ROCMTEXT+L'ROCMTEXT-RORDCOMD,R6),0(R7)                         
         BAS   RE,PUTINIOA                                                      
*                                                                               
PORCMX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER TRAILER                                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOTLR DS    0H                                                               
         USING RORDTLRD,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),ROTRRCCT+L'ROTRRCCT-RORDTLRD+1                             
         MVC   1(ROTRRCCT+L'ROTRRCCT-RORDTLRD,R6),0(R7)                         
         BAS   RE,PUTINIOA                                                      
*                                                                               
         NI    BITFLAG1,X'FF'-BF1DPEND    NO MORE PENDING DARE RECORDS          
*                                                                               
         L     R6,AWRKRIOA                                                      
         OC    0(2,R6),0(R6)       ENCOUNTERED AN ERROR BEFORE?                 
         BZ    POTLR50             YES                                          
*                                                                               
         LA    R6,2(R6)                                                         
         CLC   1(L'RORJTID,R6),=C'ORDREJ'                                       
         BNE   POTLR20                                                          
         GOTO1 =A(REJECT),DMCB,(RC),RR=RELO                                     
         B     POTLR50                                                          
*                                                                               
POTLR20  CLC   1(L'ROCFTID,R6),=C'ORDCFM'                                       
         BNE   POTLR30                                                          
         GOTO1 =A(CONFIRM),DMCB,(RC),RR=RELO                                    
         B     POTLR50                                                          
*                                                                               
POTLR30  GOTO1 =A(SNDERROR),DMCB,REFTLRWR,RR=RELO  FOR WRONG REC TYPE           
         B     POTLRNO                                                          
*                                                                               
POTLR50  L     RE,AWRKRIOA         RESET THE WORKER IOAREA                      
         LH    RF,=Y(WRECQLNQ)                                                  
         XCEFL                                                                  
         L     RE,AWRKRIOA                                                      
         MVI   1(RE),2                                                          
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
POTLRYES B     YES                                                              
*                                                                               
POTLRNO  B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER CONFIRMATION                                              
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOCFM DS    0H                                                               
         USING RORDCFMD,R7                                                      
         MVC   QREPCON,ROCFRPCN                                                 
         MVC   QRETURN,ROCFRTRN                                                 
*                                                                               
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),ROCFRTRN+L'ROCFRTRN-RORDCFMD+1                             
         MVC   1(ROCFRTRN+L'ROCFRTRN-RORDCFMD,R6),0(R7)                         
         BAS   RE,PUTINIOA                                                      
*                                                                               
         OI    BITFLAG1,BF1DPEND   GOT A PENDING DARE RECORD                    
*                                                                               
POCFMX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER LINE EQUIVALENTS                                          
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOLIN DS    0H                                                               
         USING RORDLIND,R7                                                      
         LA    RF,L'ROLNBLIN       MAKE SURE AGENCY BUYLINE NUMERIC             
         LA    RE,ROLNBLIN                                                      
POLIN10  CLI   0(RE),C'0'                                                       
         BL    POLIN20                                                          
         CLI   0(RE),C'9'                                                       
         BH    POLIN20                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,POLIN10                                                       
         B     POLIN30                                                          
*                                                                               
POLIN20  L     R1,AWRKRIOA                                                      
         LR    R0,R7                                                            
         LA    R7,3(R1)                                                         
         GOTO1 =A(SNDERROR),DMCB,REFBDABL,RR=RELO    BAD AGENCY BUYLINE         
         LR    R7,R0                                                            
         L     R1,AWRKRIOA         CLEAR THIS SO TRAILER KNOWS WE               
         XC    0(2,R1),0(R1)         HAVE AN ERROR                              
         B     POLINX                                                           
*                                                                               
POLIN30  PACK  DUB,ROLNBLIN        AGENCY BUYLINE IS FROM 1-255                 
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    POLIN20                                                          
         CH    R1,=H'255'                                                       
         BH    POLIN20                                                          
*                                                                               
         L     R6,AIO1             SAVE THIS FIRST                              
****     MVI   0(R6),ROLNRLIN+L'ROLNRLIN-RORDLIND+1                             
         MVC   1(ROLNRLIN+L'ROLNRLIN-RORDLIND,R6),0(R7)                         
         LA    R6,ROLNRLIN+L'ROLNRLIN-RORDLIND(R6)   R6 IS A(LAST CHAR)         
POLIN35  CLI   0(R6),C' '          TO COMPRESS TO AVOID OVERFLOW                
         BH    *+8                                                              
         BCT   R6,POLIN35          SAFE BECAUSE OF THE AGY BUYLINE              
*                                                                               
         LA    R1,1(R6)            BECAUSE OF THE LAST CHARACTER                
         L     R6,AIO1                                                          
         SR    R1,R6                                                            
         STC   R1,0(R6)                                                         
         BAS   RE,PUTINIOA                                                      
*                                                                               
POLINX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ERROR NOTIFICATION FOR AGENCY                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCERNT DS    0H                                                               
         GOTO1 =A(ERRNOTCE),DMCB,(RC),RR=RELO                                   
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE AGENCY RECALL FOR REP                                           
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCARCL DS    0H                                                               
         GOTO1 =A(AGYRCALL),DMCB,(RC),RR=RELO                                   
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE ORDER RECALL ACKNOWLEDGEMENT                                    
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCORAK DS    0H                                                               
         GOTO1 =A(RCLAKNWL),DMCB,(RC),RR=RELO                                   
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE MAKEGOOD REP OK (MAKEGOOD CONFIRMATION)                         
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCMGOK DS    0H                                                               
         GOTO1 =A(MKGDCNFM),DMCB,(RC),RR=RELO                                   
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE MAKEGOOD REP CANCELLATION                                       
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCMGCN DS    0H                                                               
         GOTO1 =A(MKGDCNCL),DMCB,(RC),RR=RELO                                   
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE DARE FAX DELIVERY NOTIFICATION                                  
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCDFAX DS    0H                                                               
         GOTO1 =A(DAREDLFX),DMCB,(RC),RR=RELO                                   
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE DARE FAX CANCELLATION                                           
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCCNFX DS    0H                                                               
         GOTO1 =A(DARECNFX),DMCB,(RC),RR=RELO                                   
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*  READ BLOCK                                                                   
***********************************************************************         
READBLK  NTR1                                                                   
         LH    R1,EDCTFDSK         IS TRACK FOR THIS DAY                        
         LA    R1,1(R1)                                                         
         CH    R1,EDCTFLST                                                      
         BH    RDBLKNO                                                          
*                                                                               
         GOTO1 VDADDS,DMCB,RDID,AHUGEBLK,0,EDICTFL,EDCTFDSK,0                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   9(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RDBLKYES B     YES                                                              
*                                                                               
RDBLKNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES INFORMATION FOR DARE IN THE SSB                          
*                                                                               
* CALLED BY PROCESS WHEN IT ENCOUNTERS A EDICT RECORD NOT IN TODAY'S            
* MONTH                                                                         
***********************************************************************         
UPDATSSB NTR1                                                                   
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
*                                                                               
         MVC   SSBDARDT,BTODAY     GOT SOMETHING FOR TODAY                      
         TM    BITFLAG1,BF1YSDAY                                                
         BZ    *+10                                                             
         MVC   SSBDARDT,PRIORDAT                                                
*                                                                               
         LA    R1,SSBDAREA                                                      
         CLI   WHCHEDCT,C'A'                                                    
         BE    *+8                                                              
         LA    R1,SSBDARER                                                      
         MVC   0(3,R1),EDCTFDSK    SAVE LAST TRACK AND BLOCK READ               
         ZIC   RF,RECNUM                                                        
         BCTR  RF,0                -1 BECAUSE WE FOUND SOME OLD REC             
         STC   RF,3(R1)            AND RECORD NUMBER WE'RE UPTO                 
         DROP  RE                                                               
*                                                                               
USSBX    B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE UPDATES INFORMATION FOR DARE IN THE SSB                          
*                                                                               
* CALLED BY THE OBJECTS SO RECNUM IS CORRECT NOT ONE OFF                        
***********************************************************************         
UPDATSB1 NTR1                                                                   
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
*                                                                               
         MVC   SSBDARDT,BTODAY     GOT SOMETHING FOR TODAY                      
         TM    BITFLAG1,BF1YSDAY                                                
         BZ    *+10                                                             
         MVC   SSBDARDT,PRIORDAT                                                
*                                                                               
         LA    R1,SSBDAREA                                                      
         CLI   WHCHEDCT,C'A'                                                    
         BE    *+8                                                              
         LA    R1,SSBDARER                                                      
         MVC   0(3,R1),EDCTFDSK    SAVE LAST TRACK AND BLOCK READ               
         MVC   3(1,R1),RECNUM      AND RECORD NUMBER WE'RE UPTO                 
         DROP  RE                                                               
*                                                                               
USSB1X   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SWITCHES CONTROL TO THE CORRECT SPOT SYSTEM                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
*              USERID              SET TO ID OF SPOT AGENCY                     
*                                                                               
* ON EXIT:     SPTSENUM            AGENCY'S SPOT SENUM                          
*              DLNFRID             ID OF WHERE DLNNOT CAME FROM (REP)           
***********************************************************************         
SWTCHCTL NTR1                                                                   
         GOTO1 =A(SWTCHSPT),RR=RELO                                             
SWCTLX   B     XIT                                                              
*                                                                               
***********************************************************************         
* SWITCHES CONTROL TO THE REP  SYSTEM                                           
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
*                                                                               
* ON EXIT:     SPTSENUM            REP'S SENUM                                  
***********************************************************************         
SWTCHCTR NTR1                                                                   
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             NO                                           
*                                                                               
         XC    KEY,KEY             LOOK FOR THE ID RECORD                       
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,REP          INSERT REP CODE                              
         DROP  R4                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,KEY,AIO1                             
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         CLC   CTIKEY,KEY          ERR IF WE CAN'T FIND THE ACCESS REC          
         BE    SWCTR05                                                          
SWCTRERR CLC   =C'ERRNOT',0(R7)           ERROR FROM ERRNOT?                    
         BE    SWCTRNO                                                          
         GOTO1 =A(SNDERROR),DMCB,REFRIDNV,RR=RELO  NO, REP ID NOT VALID         
         B     SWCTRNO                                                          
*                                                                               
SWCTR05  SR    R0,R0                                                            
         LA    R6,CTIDATA                                                       
         USING CTSYSD,R6                                                        
SWCTR10  CLI   0(R6),0                                                          
         BE    SWCTRERR            ERROR IF NO REP SYS ELEM                     
         CLI   0(R6),X'21'                                                      
         BNE   *+12                                                             
         CLI   CTSYSNUM,8          LOOK FOR REP SYSTEM AUTH                     
         BE    SWCTR20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SWCTR10                                                          
*                                                                               
SWCTR20  MVC   SPTSENUM,CTSYSSE                                                 
         DROP  R6                                                               
*                                                                               
         L     R1,VSELIST                                                       
         ZICM  RE,0(R1),2          SET UP BXLE USING (RE,RF)                    
         ICM   RF,15,2(R1)                                                      
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
*                                                                               
SWCTR30  CLC   SESYS,SPTSENUM                                                   
         BNE   SWCTR33                                                          
         TM    SEIND,SEISETRO+SEIRONLY    READ-ONLY STATUS?                     
         BNZ   SWCTRNO                                                          
         B     SWCTR36                                                          
*                                                                               
SWCTR33  BXLE  R1,RE,SWCTR30       CHECK NEXT ENTRY IN SELIST                   
         B     SWCTRNO                                                          
*                                                                               
SWCTR36  MVC   DMCB(1),SPTSENUM    SWITCH TO REP  SYSTEM                        
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BE    SWCTR40                                                          
         CLI   DMCB+4,2            SWITCHED, BUT SYSTEM NOT OPENED?             
***BAD   BE    EXITPRG             YES, SHOULD WAIT UNTIL OPENED                
***BAD   B     SWCTRERR            USER NOT AUTHORIZED                          
*                                                                               
         BNE   SWCTRERR            USER NOT AUTHORIZED                          
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             NO                                           
         B     SWCTRNO             SKIP THE DAMN ENTRY                          
*                                                                               
SWCTR40  L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA          NOW FIND POWER CODE FOR REP                  
SWCTR45  CLI   0(R6),0                                                          
         BE    SWCTRERR            ERROR IF NO REP POWER CODE ELEMENT           
         CLI   0(R6),X'06'                                                      
         BE    SWCTR50                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SWCTR45             GO BACK FOR NEXT ELEMENT                     
*                                                                               
SWCTR50  MVC   POWERCDE,2(R6)      SAVE POWER CODE                              
*                                                                               
SWCTRYES B     YES                                                              
*                                                                               
SWCTRNO  B     NO                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SWITCHES SYSTEM WHETHER THE PROGRAM HAS BEEN AUTHORIZED TO SWITCH             
* SYSTEMS OR NOT                                                                
*                                                                               
* ON ENTRY:    DMCB    BYTE  0     SYSTEM SENUM TO SWITCH TO                    
***********************************************************************         
SWTCHSYS NTR1                                                                   
         MVC   DMCB+1(3),=3X'FF'   DON'T CARE IF PROGRAM IS AUTHORIZED          
         GOTO1 VSWITCH,DMCB,,0                                                  
         CLI   DMCB+4,0            SUCCESSFUL?                                  
         BNE   SWSYSNO             NO                                           
*                                                                               
SWSYSYES B     YES                                                              
*                                                                               
SWSYSNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERTS THE 8 BYTE NUMERIC ORDER NUMBER TO A 4 BYTE BINARY CODE              
*                                                                               
* ON ENTRY:    PARAM 1             A(ORDER NUMBER IN EBCDIC FORM)               
*                                                                               
* ON EXIT:     BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
***********************************************************************         
CALCORDR NTR1                                                                   
         L     R2,DMCB                                                          
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         LR    RE,R2               MAKE SURE ORDER NUMBER IS VALID              
         LA    RF,L'RDNTORDR                                                    
CORDR10  CLI   0(RE),C'0'                                                       
         BL    CORDRNO                                                          
         CLI   0(RE),C'9'                                                       
         BH    CORDRNO                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,CORDR10                                                       
*                                                                               
         GOTO1 VHEXIN,DMCB,(R2),BINORDER,L'RDNTORDR                             
         OI    PACKOF4B+3,X'0F'    CONVERT IT TO PACK                           
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),BINORDER  STICK IN DAYS IN YEAR                    
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=X'FFFF'                                                
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=X'FFFF'                                                
*                                                                               
CORDRYES B     YES                                                              
*                                                                               
CORDRNO  CLC   =C'ERRNOT',0(R7)                                                 
         BE    NO                                                               
         GOTO1 =A(SNDERROR),DMCB,REFBDORD,RR=RELO     BAD ORDER NUMBER          
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY ORDER RECORD FOR UPDATE AND PUTS IT IN AIO1                   
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETORDER NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   GORDRNO                                                          
         CLI   DOKCMT,0            WE HAVE A COMMENT INSTEAD?                   
         BNE   GORDRNO             NO GOOD, ERROR, BETTER THAN DC H'0'          
         DROP  R4                                                               
*********                                                                       
* READ ORDER RECORD FOR UPDATE                                                  
*********                                                                       
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),SPTFIL,KEY+14,AIO1,DMWORK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GORDRYES B     YES                                                              
*                                                                               
GORDRNO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         BE    NO                                                               
         GOTO1 =A(SNDERROR),DMCB,REFORDNE,RR=RELO  ORDER DOES NOT EXIST         
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY MAKEGOOD NOTICE RECORD AND PUTS IT IN AIO1                    
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETNOTCE NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MNKEY,R4                                                         
         MVI   MNKTYPE,MNKTYPQ                                                  
         MVI   MNKSUBTY,MNKSTYPQ                                                
         MVC   MNKAGMD,BAGYMD                                                   
         MVC   MNKBYR,QBUYER                                                    
         MVC   MNKORDER,BINORDER                                                
         MVC   MNKGROUP,QMGGROUP                                                
         DROP  R4                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BNE   GNTCENO                                                          
*********                                                                       
* READ NOTICE RECORD FOR UPDATE                                                 
*********                                                                       
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),SPTFIL,KEY+14,AIO1,DMWORK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GNTCEYES B     YES                                                              
*                                                                               
GNTCENO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         BE    NO                                                               
         GOTO1 =A(SNDERROR),DMCB,REFMGDNE,RR=RELO  OFFER DOES NOT EXIST         
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY MAKEGOOD OFFER RECORD AND PUTS IT IN AIO1                     
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETOFFER NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MOKEY,R4                                                         
         MVI   MOKTYPE,MOKTYPQ                                                  
         MVI   MOKSUBTY,MOKSTYPQ                                                
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,BINORDER                                                
         MVC   MOKMGCD,QMGGROUP                                                 
         DROP  R4                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'MOKEY),KEYSAVE                                             
         BNE   GOFFRNO                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,GETREC,SPTFIL,KEY+14,AIO1,DMWORK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GOFFRYES B     YES                                                              
*                                                                               
GOFFRNO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         BE    NO                                                               
         GOTO1 =A(SNDERROR),DMCB,REFMGDNE,RR=RELO  OFFER DOES NOT EXIST         
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY HEADER RECORD FOR UPDATE AND PUTS IT IN AIO1                  
*                                                                               
* ON ENTRY:    PARAM1              STATION CODE                                 
*              PARAM2              AGENCY ROUTING CODE                          
*              PARAM3              DARE ORDER #                                 
*                                                                               
* ON EXIT:     CONDITION CODE      NE - RECORD DOESN'T EXIST                    
*                                  EQ - RECORD IS IN AIO1                       
***********************************************************************         
GETRPORD NTR1                                                                   
         XC    KEY,KEY             SETUP THE AGENCY HEADER KEY                  
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
         MVI   RDARKTYP,X'41'      INSERT KEY TYPE                              
         MVI   RDARKRT,X'10'       AGENCY HEADER RECORD TYPE                    
         MVC   RDARKREP,POWERCDE   INSERT POWER CODE                            
         L     R2,0(R1)            R2 = A(STATION CODE)                         
         MVC   RDARKSTA,0(R2)                                                   
         MVI   RDARKSTA+5,C' '     BECAUSE SRRDR00 DOES IT                      
         L     R2,4(R1)            R2 = A(AGENCY ROUTING CODE)                  
         MVC   RDARKAGY(L'RDARKAGY+L'RDARKAOF),0(R2)                            
         L     R2,8(R1)            R2 = A(DARE ORDER #)                         
         GOTO1 VHEXIN,DMCB,0(R2),RDARKORD,L'RDNTORDR                            
         DROP  R4                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,=C'REPDIR',KEY,KEY                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE   DID WE FIND IT?                  
         BNE   GRPORNO                         NO                               
*********                                                                       
* READ AGENCY HEADER RECORD FOR UDPATE INTO AIO1                                
*********                                                                       
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEY+28,AIO1,   X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GRPORYES B     YES                                                              
*                                                                               
GRPORNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* TEST TO SEE IF AGENCY GOT SPANKED FROM ONE ADV TO ANOTHER                     
***********************************************************************         
SPANKED  NTR1                                                                   
         LA    RE,SPANKTBL                                                      
SPNK10   CLI   0(RE),X'FF'                                                      
         BE    SPNKEXIT                                                         
*                                                                               
         CLC   AGENCY,0(RE)        MATCHES AN AGENCY THAT GOT SPANKED?          
         BNE   SPNK20                                                           
         CLC   THESYSID,2(RE)      ARE WE ON THE OLD ADV?                       
         BE    SPNKNO                                                           
         CLC   THESYSID,3(RE)      ARE WE ON THE NEW ADV?                       
         BE    SPNKYES                                                          
         B     SPNKNO                                                           
*                                                                               
SPNK20   LA    RE,4(RE)            LOOP THROUGH ALL SPANKED AGENCIES            
         B     SPNK10                                                           
*                                                                               
SPNKYES  MVC   BYTE,3(RE)                                                       
         B     *+8                                                              
*                                                                               
SPNKNO   MVI   BYTE,0                                                           
*                                                                               
SPNKEXIT B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS GETS THE CLIENT SO WE CAN LOOK UP THE PRODUCT CODES                      
***********************************************************************         
GETCLTRC NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTHDRD,R4                                                       
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         DROP  R4                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   GCRECNO                                                          
*********                                                                       
* READ CLIENT RECORD                                                            
*********                                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),SPTFIL,KEY+14,AIO1,DMWORK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GCRECYES B     YES                                                              
*                                                                               
GCRECNO  B     NO                                                               
         SPACE 2                                                                
***********************************************************************         
* THIS GETS THE EBCDIC PRODUCT CODE FOR THE BINARY PRODUCT CODE                 
*                                                                               
* ON ENTRY:    PARAM 1  BYTE 0     BINARY PRODUCT CODE                          
*                       BYTES 1-3  A(EBCDIC PRODUCT CODE)                       
***********************************************************************         
GETQPRD  NTR1                                                                   
         CLI   0(R1),0             NO BINARY CODE?                              
         BNE   GQPRD00                                                          
*                                                                               
         L     RF,0(R1)                                                         
         LA    RF,0(RF)                                                         
         XC    0(3,RF),0(RF)       THEN CLEAR OUT THIS FIELD                    
         B     GQPRDX                                                           
*                                                                               
GQPRD00  L     R6,AIO1             ELSE SEARCH THROUGH CLIENT PRD LIST          
         USING CLTHDRD,R6                                                       
         LA    RE,CLIST                                                         
GQPRD10  OC    0(3,RE),0(RE)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   3(1,RE),0(R1)                                                    
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     GQPRD10                                                          
*                                                                               
         L     RF,0(R1)                                                         
         LA    RF,0(RF)                                                         
         MVC   0(3,RF),0(RE)                                                    
*                                                                               
GQPRDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* COMMON EXIT POINTS                                                            
***********************************************************************         
YES      SR    RC,RC               SET CC TO EQ                                 
NO       LTR   RC,RC               SET CC TO NEQ                                
XIT      XIT1                      RETURN TO CALLER                             
*                                                                               
EXITPRG  L     RD,SAVERD           EXIT THE PROGRAM                             
         B     XIT                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
BNEXTEL  CLI   0(R6),0                                                          
         BE    BNEXTELX                                                         
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
BNEXTEL2 CLI   0(R6),0                                                          
         BE    BNEXTELX                                                         
         CLC   ELCDLO,0(R6)                                                     
         BH    BNEXTEL                                                          
         CLC   ELCDHI,0(R6)                                                     
         BL    BNEXTEL                                                          
         CR    RB,RB                                                            
         B     *+6                                                              
BNEXTELX LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
ENQZMSG  DC    CL40'+ENQDEQ+ MEDZ LONG UPDATE     (FACPAK)'                     
ENQXMSG  DC    CL40'+ENQDEQ+ MEDZ UPDATE ENDED    (FACPAK)'                     
ALLSPCES DC    132C' '                                                          
         SPACE 1                                                                
DMADD    DC    C'DMADD  '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMUNLK   DC    C'DMUNLK '                                                       
DMWRT    DC    C'DMWRT  '                                                       
*                                                                               
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
*                                                                               
GFILE    DC    CL8'GFILE'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
*                                                                               
SPTDIR   DC    C'SPTDIR '                                                       
SPTFIL   DC    C'SPTFIL '                                                       
CTFILE   DC    C'CTFILE '                                                       
STATION  DC    C'STATION'                                                       
         EJECT                                                                  
SPANKTBL DS    0C                                                               
         DC    C'CE',X'02',X'0C'                                                
         DC    C'SF',X'02',X'0C'                                                
         DC    C'DT',X'02',X'0C'                                                
         DC    C'TH',X'02',X'0C'                                                
         DC    C'BS',X'02',X'0C'                                                
         DC    C'DF',X'02',X'0C'                                                
         DC    C'DW',X'02',X'0C'                                                
         DC    C'BT',X'02',X'0C'                                                
         DC    C'NF',X'02',X'0C'                                                
         DC    C'TB',X'02',X'0C'                                                
         DC    C'RH',X'02',X'0C'                                                
         DC    C'XD',X'02',X'0C'                                                
         DC    X'FF'               END-OF-TABLE                                 
***********************************************************************         
* OPENS A NEW WORKER FILE FOR CREATE                                            
***********************************************************************         
WRKRCREA NTR1                                                                   
         L     R4,AIO2                                                          
         XC    0(256,R4),0(R4)     CLEAR SFH                                    
         USING WLHDRD,R4                                                        
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,SIGNON2H                                                 
         MVC   WLSYSPRG,=C'PRD'                                                 
         MVC   WLSUBPRG,SYSN1       *** FACPAK ID                               
         MVI   WLDAY,0                                                          
*                                                                               
         MVI   WLCLASS,C'T'        CLASS 'T'                                    
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXECUTION               
         MVI   WLATTB,WLATOBJ      SET OBJECT CODED DATA FLAG                   
         MVC   WLDESC,=CL16' '     FILL IN DESC '$MAD LUIDLUID   '              
         MVC   WLDESC(4),=C'$PRD'                                               
         L     RF,SRPARMSD.SRQAUTL                                              
         USING UTLD,RF                                                          
         MVC   WLDESC+5(8),TSYM                                                 
         DROP  RF                                                               
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
         MVC   WRKFILNO,WLREPRNO   EXTRACT FILE NUMBER                          
*                                                                               
WCREAX   B     YES                                                              
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* SEND DIRECTLY TO WORKER FILE                                                  
*                                                                               
* ON ENTRY:    PARAM 1             A(RECORD TO BE SENT)                         
***********************************************************************         
WRKRSEND NTR1                                                                   
         L     R4,0(R1)                                                         
*                                                                               
         LA    R3,ELEM             FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEM,ELEM                                                        
         MVC   UKUSRID,SIGNON2H    OF THE AGENCY                                
         DROP  R3                                                               
*                                  CALL DATAMGR TO CREATE FILE                  
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'WRKFILE',(R3),(R4),AWRKRBUF         
         CLI   8(R1),0                                                          
WRKRSYES BE    YES                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* CLOSE THE WORKER FILE                                                         
***********************************************************************         
WRKRCLOS NTR1                                                                   
         L     R4,AIO2                                                          
         USING WLHDRD,R4                                                        
         XC    0(WLSOFEND-WLHDRD,R4),0(R4)                                      
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
*                                                                               
         GOTO1 WRKRSEND,DMCB,AIO2                                               
*                                                                               
WRKRCYES B     YES                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FACIDTAB                                                       
       ++INCLUDE FACIDTABD                                                      
         EJECT                                                                  
DAREOBJD DSECT                                                                  
DOBJTID  DS    CL6                 TRANSMISSION ID                              
DOBJRIDN DS    XL1                 RETURN ID NUMBER                             
DOBJNXT  DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
EDICTFL  DS    A                   A(EDICT FILE)                                
DAFILT   DS    A                   DISK ADDRESS FILTER                          
DSKAD    DS    A                   LAST PAGE'S LAST DISK ADDRESS                
AIO1     DS    A                   A(IOAREA #1)                                 
AIO2     DS    A                   A(IOAREA #2)                                 
ASPLAREA DS    A                   A(SPOOL AREA)                                
AWRKRIOA DS    A                   A(IO AREA USED BY EDICT)                     
AWRKRBUF DS    A                   A(WORKER BUFFER AREA)                        
AHUGEBLK DS    A                   A(HUGE BLOCK)                                
*                                                                               
VADDAY   DS    V                                                                
VDATCON  DS    V                                                                
VGETFACT DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VLOCKET  DS    V                                                                
VRECUP   DS    V                                                                
VCLUNPK  DS    V                                                                
VMSPACK  DS    V                                                                
VMSUNPK  DS    V                                                                
VSWITCH  DS    V                                                                
*                                                                               
ASPOOL   DS    A                                                                
ASTAPACK DS    A                                                                
*                                                                               
SRPARMS  DS    8F                  SERVICE REQUEST PARAMETERS                   
EDCTFDSK DS    F                   EDICTFIL DISK ADDRESS                        
DMCB     DS    6F                                                               
FULL     DS    F                                                                
*                                                                               
EDCTFRPT DS    H                   EDICTFIL PHYSICAL RECORDS PER TRACK          
EDCTFTPD DS    H                   EDICTFIL TRACKS PER DAY                      
EDCTFATQ EQU   40                     ON ADV                                    
EDCTFRTQ EQU   40                     ON REP                                    
EDCTFTTQ EQU   2                      ON TEST                                   
EDCTFMTQ EQU   2                      ON MEL                                    
EDCTRPBQ DS    H                   EDICTFIL LOGICAL RECORDS PER BLOCK           
EDCTLRCL DS    H                   EDICTFIL LOGICAL RECORD LENGTH               
EDCTFLST DS    H                   LAST TRACK FOR GIVEN DAY                     
FRSTTRK  DS    H                   FIRST TRAC FOR GIVEN DAY                     
RECLEN   DS    H                                                                
TERMNUM  DS    H                   TERMINAL NUMBER                              
USERNUM  DS    H                   USER ID NUMBER                               
TRNNUM   DS    H                   TRANSACTION COUNT                            
HALF     DS    H                                                                
DATADISP DS    H                                                                
*                                                                               
AGENCY   DS    XL2                 AGENCY POWER CODE                            
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
USERID   DS    CL10                CONTROL USER ID                              
REP      DS    CL10                REP 'FROM ID'                                
POWERCDE DS    CL2                 POWER CODE FOR REP READ                      
BAGYMD   DS    XL1                 BINARY AGENCY/MEDIA                          
BCLT     DS    XL2                 BINARY CLIENT CODE                           
BEST     DS    XL1                 BINARY ESTIMATE                              
BMKTSTA  DS    XL5                 BINARY MARKET STATION                        
BPRD     DS    XL1                                                              
BINORDER DS    0XL4                ORDER NUMBER (FF COMPLEMENT)                 
BINORDDT DS    XL2                   (YEAR-90)*1000 + JULIAN DAY                
BINORDSQ DS    XL2                   SEQUENCE NUMBER (0-9999)                   
POLORDER DS    XL4                 POL ORDER NUMBER (FF COMPLEMENT)             
SVAPRF07 DS    XL1                 SAVED COPY OF APROF+07                       
SVCPRF00 DS    XL1                 SAVED COPY OF CPROF+00                       
SVCOFFC  DS    XL1                 CLIENT OFFICE                                
CONTNUMB DS    CL8                 CONTRACT NUMBER                              
         DS    0F                                                               
PACKOF4B DS    PL4                 PACKED NUMBER OF 4 BYTES                     
JDTTODAY DS    PL4                 JULIAN DATE OF TODAY                         
QREPCON  DS    CL8                 COPY OF REP CONTRACT NUMBER                  
QRETURN  DS    CL16                COPY OF RETURN TO SENDER DATA                
*                                                                               
QMED     DS    CL1                 EBCDIC MEDIA                                 
QBUYER   DS    CL3                        BUYER CODE                            
QCLT     DS    CL3                        CLIENT                                
QPRD1    DS    CL3                        PRODUCT 1                             
QPRD2    DS    CL3                        PRODUCT 2                             
QEST1    DS    CL3                        ESTIMATE 1                            
QSTA     DS    CL8                        STATION                               
QMGGROUP DS    CL3                        MAKEGOOD GROUP CODE                   
QFLTNUM  DS    CL2                        FLIGHT NUMBER                         
*                                                                               
ESTPW    DS    CL3                 PW PERCENTAGE                                
*                                                                               
THESYSID DS    XL1                 SAVED SYSTEM ID                              
WHCHEDCT DS    XL1                 WHICH EDICT FILE (A)DV OR (R)EP              
*                                                                               
RECSKIP  DS    XL1                 # OF RECORDS TO BUMP INTO BLOCK              
RECNUM   DS    XL1                 # OF RECORDS INTO BLOCK                      
SPTSENUM DS    XL1                 SPOT SYSTEM SENUM                            
BYTE     DS    C                                                                
*                                                                               
PIGPRD   DS    XL1                 PIGGYBACK PRODUCT BINARY CODE                
PIGEST   DS    XL1                 PIGGYBACK ESTIMATE                           
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS BIT FLAGS                            
BF1SKPTB EQU   X'80'                - SKIP TRACK/BLOCK                          
BF1SKIPB EQU   X'40'                - SKIP RECORD BUMP                          
BF1FRSTR EQU   X'20'                - FIRST RECORD IN BLOCK                     
BF1DPEND EQU   X'10'                - DARE RECORD PENDING                       
BF1NWCOM EQU   X'08'                - NEED TO ADD REP COMMENT RECORD            
BF1IGRCM EQU   X'04'                - IGNORE REST OF REP COMMENTS               
BF1PSSWD EQU   X'02'                - PASSWORD REQUIRED                         
BF1YSDAY EQU   X'01'                - USING PRIOR BUSINESS DAY'S INFO           
*                                                                               
BITFLAG2 DS    XL1                                                              
BF2ELCKD EQU   X'80'               ESTIMATE STATUS IS HOLD OR LOCKED            
BF2VARCN EQU   X'40'               THE VAR ORDER IS CONFIRMED                   
BF2ALLPD EQU   X'20'               ALL BRANDS ARE MARKED CONFIRM PENDNG         
BF2CNFCM EQU   X'10'               WE HAVE CONFIRM WITH COMMENTS                
BF2MGSEQ EQU   X'08'               WE HAVE MG SEQ NUMBER PROBLEM                
BF2PWLCK EQU   X'04'               PW BUY LOCKED                                
*                                                                               
DOBJNUMB DS    XL1                 DARE OBJECT NUMBER                           
DOBJDLNQ EQU   1                   DARE DELIVERY NOTIFICATION                   
DOBJOAPQ EQU   2                   DARE ORDER APPROVAL                          
DOBJORJQ EQU   3                   DARE ORDER REJECTION                         
DOBJOCMQ EQU   4                   DARE ORDER REJECTION COMMENT                 
DOBJOTRQ EQU   5                   DARE ORDER TRAILER                           
DOBJOCFQ EQU   6                   DARE ORDER CONFIRMATION                      
DOBJOLNQ EQU   7                   DARE ORDER LINE NUMBER EQUIVALENTS           
DOBJERRQ EQU   8                   DARE ERROR NOTIFICATION                      
DOBJARCQ EQU   9                   DARE AGENCY RECALL                           
DOBJORAQ EQU   10                  DARE ORDER RECALL ACKNOWLEDGEMENT            
DOBJMOKQ EQU   11                  MAKEGOOD CONFIRMATION                        
DOBJMCNQ EQU   12                  MAKEGOOD CANCELLATION                        
DOBJDFXQ EQU   13                  DARE FAX DELIVERY NOTIFICATION               
DOBJCFXQ EQU   14                  DARE FAX CANCELLATION                        
*                                                                               
BTODAY   DS    XL3                 TODAY'S DATE IN BINARY (YMD)                 
CTODAY   DS    CL8                 TODAY'S DATE IN CHAR FORMAT                  
CTOMORW  DS    CL8                 TOMORROW'S DATE IN CHAR FORMAT               
PRIORDAT DS    XL3                 YESTERDAY'S OR SOME PRIOR DAY'S DATE         
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
SYSN1    DS    CL1                 1 CHR FACPAK NAME                            
WRKFILNO DS    XL2                 WORK FILE NUMBER                             
WRKRECNO DS    F                   WORKER FILE RECORD NUMBER                    
*                                                                               
WORK     DS    XL64                                                             
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
DKEY     DS    XL64                KEY OF LAST DA RECORD ADDED                  
HEADER   DS    XL32                                                             
DMWORK   DS    12D                                                              
*                                                                               
FILENAME DS    CL7                 DMGR FILENAME                                
LASTFILE DS    CL1                 PREVIOUS FILE NUMBER                         
DALINK   DS    CL4                 DISK ADDRESS OF LAST ADDREC                  
DDA      DS    CL4                 DISK ADDRESS OF GETREC                       
DALAST   DS    CL1                 LAST DA FILE ADDED TO                        
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
LASTACTN DS    CL1                 LAST TYPE (COPY CHNG ADD)                    
DLNFRID  DS    XL2                 DELIVERY NOTICE SENDER ID NUM                
ROUTNGCD DS    CL5                 ROUTING CODE                                 
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
ELCODE   DS    XL1                                                              
REVISION DS    XL1                 REVISION NUMBER                              
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
NUMSPOTS DS    XL1                                                              
RECCOUNT DS    F                   RECORD COUNT                                 
*                                                                               
DEMSAVE  DS    XL6                 DEMO EXIT SAVE AREA FOR DEMPROS              
*                                                                               
SAVEBUFF DS    0XL12                                                            
SBVPQTAB DS    A                   A(PQTAB)                                     
SBVPQFIL DS    A                   A(PQFILE DTF)                                
SBSAVE   DS    A                   DISPLACEMENT TO START OF SAVE AREA           
*                                                                               
SAVESIN  DS    F                                                                
NEXTSIN  DS    F                                                                
QSIN     DS    CL6                                                              
PRTQID   DS    CL8                                                              
*                                                                               
PROFDAR  DS    CL16                DAR PROFILE                                  
PDARONHD EQU   PROFDAR+0            - ON-HOLD IF MKGD OKAYED BY REP?            
*                                                                               
REMUSER  DS    CL3                                                              
BIGSPLKY DS    CL144                                                            
*                                                                               
ELEM     DS    CL256                                                            
*                                                                               
IOA1     DS    6000C               I/O AREA                                     
IOA2     DS    6000C               I/O AREA                                     
*                                                                               
SPULAREA DS    XL3200                                                           
*                                                                               
WRKRIOA  DS    14336C              BYTES 0-1   = LENGTH OF RECORD               
*                                  BYTE  2     = L(DATA)+1                      
*                                  BYTES 3-??  = DATA                           
WRECQLNQ EQU   *-WRKRIOA                                                        
*                                                                               
HUGEBLCK DS    18432C              HUGE OUTPUT BLOCK                            
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMWRKRD                                                                       
* DMWRKRK                                                                       
* FADSECTS                                                                      
* FAPQPL                                                                        
* CTGENFILE                                                                     
* DMGREQUS                                                                      
* DMPRTQD                                                                       
* DMPRTQS                                                                       
* DMPRTQK   <--- PREFIXED WITH 'SR'                                             
* DDCOMFACS                                                                     
* DMFILTABD                                                                     
* TASYSWORKD                                                                    
* DMREQHDRA                                                                     
* DDEDICTFIL                                                                    
* FAFACTS                                                                       
* SPTRPAT                                                                       
* SPTRSHIP                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENDARE                                                      
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMPRTQS                                                        
*PREFIX=SR                                                                      
       ++INCLUDE DMPRTQK                                                        
*PREFIX=                                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMFILTABD                                                      
       ++INCLUDE TASYSWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
       ++INCLUDE DMWRKFL                                                        
*********INCLUDE DMWRKFK                                                        
       ++INCLUDE DDEDICTFIL                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPTRPAT                                                        
       ++INCLUDE SPTRSHIP                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SRDARFFD                                                       
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
RDARRECD DSECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
RMKGRECD DSECT                                                                  
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE SPDARMKGDD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKN                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKO                                                     
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
LAYOUTD  DSECT                     LAYOUT FOR PROD / JOB                        
LAYCTRY  DS    CL1                 COUNTRY                                      
LAYCUSA  EQU   C'0'                USA                                          
LAYCUK   EQU   C'1'                UK                                           
LAYCGER  EQU   C'2'                GERMANY                                      
LAYACT   DS    CL1                 ACTION                                       
LAYAADD  EQU   C'A'                ADD                                          
LAYAAUTO EQU   C'B'                AUTO                                         
LAYACHA  EQU   C'C'                CHANGE                                       
LAYCLT   DS    CL6                 CLIENT                                       
LAYPRD   DS    CL6                 PRODUCT                                      
LAYJOB   DS    CL6                 JOB                                          
LAYJOB   DS    CL36                JOBNAME                                      
LAYFLT1  DS    CL1                 FILTER 1-5                                   
LAYFLT2  DS    CL1                                                              
LAYFLT3  DS    CL1                                                              
LAYFLT4  DS    CL1                                                              
LAYFLT5  DS    CL1                                                              
LAYODATE DS    CL8                 OPEN DATE                                    
LAYCDATE DS    CL8                 CLOSE DATE                                   
LAYPOBLL DS    CL49                PRINT ON BILL                                
LAYUFLD1 DS    CL30                USER FIELDS 1-10                             
LAYUFLD2 DS    CL30                                                             
LAYUFLD3 DS    CL30                                                             
LAYUFLD4 DS    CL30                                                             
LAYUFLD5 DS    CL30                                                             
LAYUFLD6 DS    CL30                                                             
LAYUFLD7 DS    CL30                                                             
LAYUFLD8 DS    CL30                                                             
LAYUFLD9 DS    CL30                                                             
LAYUFLDA DS    CL30                                                             
LAYOINF1 DS    CL50                OTHER INFO 1-3                               
LAYOINF2 DS    CL50                                                             
LAYOINF3 DS    CL50                                                             
LAYJBCM1 DS    CL50                JOB COMMENTS 1-3                             
LAYJBCM2 DS    CL50                                                             
LAYJBCM3 DS    CL50                                                             
LAYEND   EQU   *                                                                
*                                                                               
BUYINFD  DSECT                                                                  
STRTIM   DS    XL2                                                              
ENDTIM   DS    XL2                                                              
SPTLEN   DS    XL1                                                              
SPTCST   DS    XL3                                                              
SPTDAT   DS    XL2                                                              
LINNO    DS    XL1                                                              
BYINFLN  EQU   *-STRTIM                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SRPRD00   02/14/13'                                      
         END                                                                    
