*          DATA SET T40A00     AT LEVEL 073 AS OF 05/01/02                      
*PHASE T40A00A                                                                  
*INCLUDE PUBVAL                                                                 
*INCLUDE PUBEDIT                                                                
         TITLE 'T40A00 - PUB LIST PROGRAM - BASE'                               
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
* SMYE 11/27/00 "PROGRAM REPLACED BY SFM" MESSAGE DISPLAYED                     
*                ALL ACTIVITY DISABLED                                          
*                                                                               
*   BPLA 10/98  FIX BUG - CURSOR TO FIRST PUB (NOT ACTION)                      
*               TO CONTINUE DISPLAY                                             
*                                                                               
*   BPLA 3/97   ALPHA TEST FOR CLIENT NO-OPED                                   
*                                                                               
T40A00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1670,T40A00                                                      
         SPACE 2                                                                
         USING GENOLD,RC                                                        
         USING T40AFFD,RA                                                       
         BAS   RE,INITL                                                         
         RELOC RELO                                                             
*                                                                               
         B     EXIT        NO ACTIVITY - DEACTIVATED WEEK OF 11/27/00           
*                                                                               
         SPACE 2                                                                
         LR    R2,RC                                                            
         AH    R2,=Y(HLDWRKS-GENOLD)                                            
         ST    R2,AHLDWRKS                                                      
         LR    R2,RC                                                            
         AH    R2,=Y(PUBIO-GENOLD)                                              
         ST    R2,APUBIO                                                        
         LA    R2,LSTSTRTH                                                      
         ST    R2,ANXTOUT                                                       
         LA    R2,PUBBUFF+2                                                     
         ST    R2,ANXTBUF                                                       
         L     R2,=V(PUBVAL)                                                    
         A     R2,RELO                                                          
         ST    R2,VPUBVAL                                                       
         L     R2,=V(PUBEDIT)                                                   
         A     R2,RELO                                                          
         ST    R2,VPUBED                                                        
         MVI   PUBBUFF+1,2         INITIALIZE BUFFER LENGTH                     
*                                  CLEAR DESC FLD                               
         EJECT                                                                  
*                                  EDIT MEDIA                                   
         SPACE 2                                                                
MED      LA    R2,LSTMEDH                                                       
         LA    R3,MEDERR                                                        
         TM    4(R2),X'20'         PREV VAL.                                    
         BNZ   CLT                                                              
         XC    LSTMEDN,LSTMEDN                                                  
         FOUT  LSTMEDNH                                                         
         XC    LSTCLTN,LSTCLTN                                                  
         FOUT  LSTCLTNH                                                         
         CLI   5(R2),1             LENGTH                                       
         BNE   ERROR                                                            
         TM    4(R2),X'04'         ALPHA                                        
         BZ    ERROR                                                            
         LA    R9,KEY                                                           
         USING PAGYREC,R9                                                       
*                                  AGY HEADER KEY                               
         MVC   PAGYKAGY,AGYALPHA                                                
         MVC   PAGYKMED,LSTMED                                                  
         MVI   PAGYKRCD,X'01'                                                   
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         LA    R9,IOAREA                                                        
         MVC   LSTMEDN,PAGYMED                                                  
         MVC   SVAGPROF,PAGYPROF                                                
         OI    4(R2),X'20'                                                      
         NI    LSTCLTH+4,X'DF'                                                  
         NI    LSTLSTH+4,X'DF'                                                  
         EJECT                                                                  
*                                  EDIT CLIENT                                  
         SPACE 2                                                                
CLT      LA    R2,LSTCLTH                                                       
         LA    R3,CLTERR                                                        
         TM    4(R2),X'20'         PREV VAL                                     
         BNZ   ACT                                                              
         XC    LSTCLTN,LSTCLTN                                                  
         FOUT  LSTCLTNH                                                         
         CLI   5(R2),0                                                          
         BNE   CLT4                                                             
         MVC   LSTCLT(3),=C'ZZZ'                                                
         FOUT  (R2)                                                             
CLT2     MVC   LSTCLTN,=CL20'ALL CLIENTS'                                       
         B     CLT6                                                             
CLT4     CLI   5(R2),2                                                          
         BL    ERROR                                                            
***                                ALPHA TEST NO-OPED 3/17/97                   
****     TM    4(R2),X'04'         ALPHA                                        
****     BZ    ERROR                                                            
         OC    LSTCLT(3),=3C' '                                                 
         CLC   LSTCLT(3),=C'ZZZ'                                                
         BE    CLT2                                                             
*                                  CLT HEADER KEY                               
         LA    R9,KEY                                                           
         USING PCLTREC,R9                                                       
         XC    KEY,KEY                                                          
         MVC   PCLTKAGY,AGYALPHA                                                
         MVC   PCLTKMED,LSTMED                                                  
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,LSTCLT                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         LA    R9,IOAREA                                                        
         MVC   LSTCLTN,PCLTNAME                                                 
CLT6     EQU   *                                                                
         OI    4(R2),X'20'                                                      
         NI    LSTLSTH+4,X'DF'                                                  
         EJECT                                                                  
*                                  EDIT ACTION                                  
         SPACE 2                                                                
ACT      LA    R2,LSTACTH                                                       
         LA    R3,ACTERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,ACTLST                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         SR    R8,R8                                                            
         IC    R8,5(R2)                                                         
         BCTR  R8,R0                                                            
         EX    R8,*+16                                                          
         BE    ACT2                                                             
         BXLE  R5,R6,*-8                                                        
         B     ERROR                                                            
         CLC   LSTACT(0),0(R5)          EXECUTED                                
ACT2     CLI   LSTACT,C'C'                                                      
         BNE   ACT4                                                             
         OC    DSPFST,DSPFST                                                    
         BNZ   ACT6                                                             
         LA    R3,CHNGERR          CHANGE NOT AFTER DISPLAY                     
         B     ERROR                                                            
ACT4     EQU   *                                                                
         CLI   LSTACT,C'D'         IS THIS ACTION DISPLAY                       
         BNE   ACT6                                                             
         CLC   LSTMSG(L'OKMSG),OKMSG    WAS LAST ACTION COMPLETED?              
         BNE   ACT6                                                             
         OC    DSPFST,DSPFST       WAS LAST ACTION DISPLAY                      
         BNZ   ACT6                                                             
         NI    LSTLSTH+4,X'DF'    UNVALIDATE LIST CODE                          
*                                 SO DISPLAY WILL START AFRESH                  
ACT6     EQU   *                                                                
         MVC   LSTACT,0(R5)                                                     
         FOUT  (R2)                                                             
*                                  CLEAR SCREEN ON DISPLAY                      
         CLI   LSTACT,C'D'                                                      
         BNE   STRT                                                             
         XC    LSTDESC,LSTDESC                                                  
         FOUT  LSTDESCH                                                         
         SR    R4,R4                                                            
         LA    R2,LSTDATAH                                                      
         LA    R3,LSTDUMH                                                       
ACT8     IC    R4,0(R2)                                                         
         S     R4,=F'9'                                                         
         EX    R4,VAROC                                                         
         BZ    ACT10                                                            
         EX    R4,VARXC                                                         
         FOUT  (R2)                                                             
ACT10    LA    R2,9(R2,R4)         BUMP                                         
         CR    R2,R3               CHECK END                                    
         BL    ACT8                                                             
         B     STRT                                                             
VAROC    OC    8(0,R2),8(R2)                                                    
VARXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
*                                  EDIT START FIELD                             
         SPACE 2                                                                
STRT     EQU   *                                                                
         CLI   LSTACT,C'C'                                                      
         BE    LIST                                                             
         XC    DSPFST,DSPFST                                                    
         CLI   LSTACT,C'D'                                                      
         BE    *+14                                                             
         XC    DSPLAST,DSPLAST                                                  
         B     LIST                                                             
*                                                                               
         TM    LSTLSTH+4,X'20'     SEE IF LIST PREVOUSULY VALIDATED             
         BNO   STRT1               IGNORE INPUT HERE                            
*                                                                               
         LA    R3,STRTERR                                                       
         LA    R2,LSTSTRTH                                                      
         CLC   LSTSTRT(4),=C'NEXT'                                              
         BE    STRT6                                                            
         CLI   5(R2),0                                                          
         BNZ   *+14                                                             
STRT1    XC    STRTNUM(8),STRTNUM  CLEAR PUB + NUM                              
         B     STRT4                                                            
*                                                                               
         CLI   5(R2),4                                                          
         BL    STRT2                                                            
*                                  ASSUME INPUT TO BE PUB                       
         IC    R8,5(R2)                                                         
         GOTO1 VPUBVAL,DMCB,((R8),LSTSTRT),WORK                                 
         SPACE 2                                                                
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   STRTPUB,WORK                                                     
         XC    STRTNUM,STRTNUM                                                  
         B     STRT4                                                            
*                                  ASSUME INPUT TO BE REF. NO.                  
STRT2    BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STH   R0,STRTNUM                                                       
         XC    STRTPUB,STRTPUB                                                  
STRT4    EQU   *                                                                
         B     LIST                                                             
STRT6    OC    DSPLAST,DSPLAST                                                  
         BNZ   *+12                                                             
         LA    R3,NXTERR                                                        
         B     ERROR                                                            
         IC    R1,DSPLAST+5                                                     
         LA    R1,1(R1)                                                         
         STC   R1,DSPLAST+5                                                     
         MVC   STRTPUB,DSPLAST                                                  
         XC    DSPFST(12),DSPFST                                                
         EJECT                                                                  
*                                  EDIT LIST CODE                               
LIST     LA    R2,LSTLSTH                                                       
         TM    4(R2),X'20'         SEE IF PREV VALIDATED                        
         BNZ   LR1                                                              
         XC    STRTPUB,STRTPUB     FOR NEW LIST CLEAR STRTPUB                   
LR1      LA    R3,LISTERR                                                       
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
*                                                                               
         CLI   5(R2),3                                                          
         BNH   LR2                                                              
         XC    LSTMSG,LSTMSG                                                    
         MVC   LSTMSG(36),=C'**ERROR - MAXIMUM INPUT 3 CHARACTERS'              
         FOUT  LSTMSGH                                                          
         B     EXIT                                                             
*                                                                               
LR2      OC    LSTLST(3),=3C' '                                                 
         OI    4(R2),X'20'         SET PREV VALIDATED                           
******************************     ONLY FOR CLEARING STRTPUB                    
*                                  BUILD LIST KEY                               
         LA    R9,KEY                                                           
         USING PLISREC,R9                                                       
         XC    KEY,KEY                                                          
         MVC   PLISKAGY,AGYALPHA                                                
         MVC   PLISKMED,LSTMED                                                  
         MVI   PLISKRCD,X'17'                                                   
         MVC   PLISKCLT,LSTCLT                                                  
         MVC   PLISKCOD,LSTLST                                                  
         MVI   LINCK,1                                                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
LR4      BAS   RE,SEQ                                                           
         CLC   KEY(10),KEYSAVE                                                  
         BNE   LR12                                                             
         CLC   PLISKLIN,LINCK                                                   
         BE    *+6                                                              
         DC    H'0'                INVALID RECORD SET                           
         SR    R4,R4                                                            
         IC    R4,LINCK                                                         
         SLL   R4,3                *8                                           
         LA    R4,RECWK-8(R4)      POINT TO RIGHT RECWK                         
         USING RECWKD,R4                                                        
         MVC   RECDSK,KEY+27                                                    
         OI    RECSTAT,X'80'       HAVE RECORD                                  
         TM    DMCB+8,X'02'        TEST DELETED                                 
         BZ    LR6                                                              
         OI    RECSTAT,X'40'                                                    
         B     LR7                                                              
LR6      CLI   DELSW,0                                                          
         BE    *+6                                                              
         DC    H'0'                INVALID RECORD SET                           
LR7      BAS   RE,GETREC                                                        
         BAS   RE,BUFFIN                                                        
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
LR8      CLI   LINCK,5                                                          
         BE    LR12                                                             
         IC    R1,LINCK                                                         
         LA    R1,1(R1)                                                         
         STC   R1,LINCK                                                         
         B     LR4                                                              
LR12     LA    R3,RECERR                                                        
         LA    R4,RECWK                                                         
         TM    RECSTAT,X'C0'                                                    
         BM    LR16                                                             
         CLI   LSTACT,C'D'                                                      
         BE    ERROR               RECORD(S) NOT FOUND                          
         CLI   LSTACT,C'C'                                                      
         BE    ERROR                                                            
*                                                                               
LR16     CLI   LSTACT,C'D'                                                      
         BNE   LR20                                                             
         XC    LSTMSG,LSTMSG                                                    
         OC    DSPFST,DSPFST                                                    
         BNZ   *+12                                                             
         LA    R3,PUBERR4                                                       
         B     ERROR                                                            
         MVC   LSTMSG(L'OKMSG),OKMSG         ALL DISPLAYED                      
         LA    R2,LSTACTH              CURSOR TO ACTION                         
         CLI   MORESW,0                                                         
         BE    LR16X                                                            
         MVC   LSTMSG(L'MORMSG),MORMSG       MORE TO DISPLAY                    
         LA    R2,LSTSTRTH             CURSOR TO FIRST PUB                      
         B     LR16X                                                            
*                                                                               
LR16X    FOUT  LSTMSGH                                                          
         B     EXIT                                                             
*                                  EDIT PUBS FROM SCREEN                        
LR20     LA    R2,LSTSTRTH                                                      
*                                  CLEAR DISPLAY OF ALL LINES                   
*                                  NOT PREV. VAL.                               
LR20A    EQU   *                                                                
         TM    4(R2),X'20'                                                      
         BNZ   LR20C                                                            
*                                                                               
         LA    R4,8+L'LSTSTRT(R2)                                               
         LA    R4,8+L'LSTFILT(R4)                                               
         OC    8(L'LSTDATA,R4),8(R4)                                            
         BZ    LR20C                                                            
         XC    8(L'LSTDATA,R4),8(R4)                                            
         FOUT  (R4)                                                             
LR20C    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,LSTDUMH                                                       
         CR    R2,R0                                                            
         BL    LR20A                                                            
LR20D    LA    R2,LSTSTRTH                                                      
LR22     EQU   *                                                                
         CLI   5(R2),0                                                          
         BE    LR26                NO INPUT                                     
         IC    R4,5(R2)                                                         
         GOTO1 VPUBVAL,DMCB,((R4),8(R2)),WORK+2                                 
         CLI   DMCB,X'FF'                                                       
         BNE   *+12                                                             
         LA    R3,PUBERR                                                        
         B     ERROR                                                            
         TM    4(R2),X'20'         TEST PREV VAL                                
         BNZ   LR23                YES - DO NOT READ FRO PUB                    
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   PUBKMED,LSTMED                                                   
         MVC   PUBKPUB(6),WORK+2                                                
         MVC   PUBKAGY,AGYALPHA                                                 
         MVI   PUBKCOD,X'81'                                                    
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         BAS   RE,HIGHPUB                                                       
         B     *+8                                                              
LR22B    BAS   RE,SEQPUB                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+12                                                             
LR22C    EQU   *                                                                
         LA    R3,PUBERR2          PUB NOT FOUND                                
         B     ERROR                                                            
         CLC   PUBKAGY,AGYALPHA                                                 
         BE    LR22D                                                            
         CLI   SVAGPROF+16,C'0'                                                 
         BE    LR22C               DONT LOOK FOR SRDS                           
         CLC   PUBKAGY,=C'ZZ'                                                   
         BNE   LR22B                                                            
LR22D    CLI   PUBKCOD,X'81'       TEST BIG REC                                 
         BNE   LR22B                                                            
         OI    4(R2),X'20'         SET PREV VAL                                 
         BAS   RE,DATADISP                                                      
LR23     DS    0H                  FILTERS                                      
         MVC   WORK+8(3),=3C' '                                                 
*                                                                               
         ST    R2,FULL             SAVE FOR ERRORS                              
*                                                                               
         LA    R2,8+L'LSTSTRT(R2)                                               
         CLI   5(R2),0                                                          
         BE    LR23D                                                            
*                                                                               
         MVC   WORK+8(3),8(R2)                                                  
         OC    WORK+8(3),=3C' '                                                 
*                                                                               
LR23D    DS    0H                                                               
*                                  PUT PUB IN SEQUENCE IN BUFFER                
         LH    R7,PUBBUFF                                                       
         LA    R7,PUBBUFF+2-1(R7)                                               
         LA    R5,PUBBUFF+2                                                     
         SR    R6,R6                                                            
         LA    R3,PUBERR3                                                       
         USING PLISPBEL,R5                                                      
LR24     CLC   WORK+2(6),PLISPUB                                                
         BL    LR25                                                             
         BE    LRERR                                                            
         CLI   PLISPBEL,0                                                       
         BE    LR25                                                             
         IC    R6,1(R5)                                                         
         BXLE  R5,R6,LR24                                                       
LR25     MVI   WORK,X'20'                                                       
         MVI   WORK+1,PLPBL                                                     
         GOTO1 VRECUP,DMCB,(X'FF',PUBBUFF),WORK,(R5)                            
*                                                                               
         CLC   PUBBUFF(2),MAXBUFF                                               
         BNH   *+12                                                             
         LA    R3,OVFERR                                                        
         BE    LRERR                                                            
         OI    4(R2),X'20'                                                      
         MVI   HAVPUB,1                                                         
         SR    R0,R0                                                            
         B     LR26B               SKIP EXTRA BUMP                              
LR26     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
LR26B    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               BUMP PAST DISPLAY FLD                        
         LA    R0,LSTDUMH                                                       
         CR    R2,R0                                                            
         BL    LR22                                                             
         B     LR28                                                             
*                                                                               
LRERR    L     R2,FULL             FULL HAS CURSOR ADDDRESS                     
         B     ERROR                                                            
*                                  TODAYS DATE                                  
LR28     EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,0),(3,WORK)   TODAY                              
         LA    R1,HLDDATEL                                                      
         USING PLISDTEL,R1                                                      
         MVC   PLISDAT,WORK                                                     
*                                  IF NO DESC ENTERED IN 'ADD'                  
*                                  LEAVE PLISDESC AS IS                         
         CLI   LSTACT,C'A'                                                      
         BNE   *+12                                                             
         CLI   LSTDESCH+5,0                                                     
         BE    *+10                                                             
         MVC   PLISDESC,LSTDESC    MOVE LIST DESCRIPTION                        
*                                  NUMBER OF PUBS                               
         LH    R3,PUBBUFF                                                       
         SR    R2,R2                                                            
         LA    R4,PLPBL                                                         
         DR    R2,R4                                                            
         STH   R3,HALF                                                          
         MVC   PLISNPBS,HALF                                                    
         MVI   PLISDTEL,X'10'                                                   
         MVI   PLISDTEL+1,PLDTL                                                 
         DROP  R1                                                               
         LH    R1,PUBBUFF                                                       
         SH    R1,=H'2'                                                         
         SR    R0,R0                                                            
         LA    R2,MAXELS*PLPBL                                                  
         DR    R0,R2                                                            
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)            R1 = NO OF RECS NEEDED                       
         LTR   R1,R1               TEST ANY RECS NEEDED                         
         BZ    LR31                                                             
         LA    R4,RECWK                                                         
         USING RECWKD,R4                                                        
         OI    RECSTAT,X'20'       SET ON REC NEEDED BIT                        
         LA    R4,8(R4)                                                         
         BCT   R1,*-8                                                           
         DROP R4                                                                
         SPACE 3                                                                
LR31     EQU   *                                                                
         LA    R2,LSTSTRTH                                                      
         LA    R3,MISSERR                                                       
         CLI   LSTACT,C'A'                                                      
         BNE   *+12                                                             
         CLI   HAVPUB,1                                                         
         BNE   ERROR               NO PUBS ON ADD                               
         EJECT                                                                  
*                                  ALL OUTPUT HANDLED HERE                      
         SPACE 2                                                                
         LA    R9,IOAREA                                                        
         MVI   LINCK,1                                                          
         SR    R6,R6               RECORD COUNTER                               
         LA    R8,PUBBUFF+2        BUFFER POINTER                               
         LA    R3,RECWK                                                         
         USING RECWKD,R3                                                        
         LA    R4,8                                                             
         LA    R5,RECWK+(5*8)-1                                                 
LR32     LA    R6,1(R6)                                                         
         TM    RECSTAT,X'20'       TEST NEEDED                                  
         BNZ   LR36                YES                                          
*                                  NO                                           
         TM    RECSTAT,X'C0'       TEST DELETED                                 
         BO    LR48                YES                                          
         BZ    LR48                                                             
         LR    R1,R6                                                            
         MH    R1,=H'31'                                                        
         LA    R1,HLDDIRS-31(R1)                                                
         MVC   KEY(31),0(R1)       MOVE IN KEY                                  
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,READ             REREAD DIR BEFORE WRITE                      
         OI    KEY+25,DELBTS                                                    
         BAS   RE,WRITE                                                         
         B     LR48                                                             
LR36     XC    IOAREA(33),IOAREA                                                
         TM    RECSTAT,X'80'       TEST ALREADY HAVE                            
         BZ    LR41                NO                                           
         TM    RECSTAT,X'40'       TEST DELETED                                 
         BZ    LR40                NO                                           
         LR    R1,R6                                                            
         MH    R1,=H'31'                                                        
         LA    R1,HLDDIRS-31(R1)                                                
         MVC   KEY(31),0(R1)                                                    
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,READ             REREAD DIR BEFORE WRITE                      
         NI    KEY+25,UNDELBTS                                                  
         BAS   RE,WRITE            UNDELETE POINTER                             
LR40     LR    R1,R6                                                            
         MH    R1,=H'33'                                                        
         LA    R1,HLDKEYS-33(R1)                                                
         MVC   IOAREA(33),0(R1)                                                 
         NI    PLISCTL,UNDELBTS                                                 
LR41     MVC   PLISLEN,=H'33'                                                   
*                                  ADD DATE ELEM                                
         CLI   LINCK,1             ONLY IF FIRST REC                            
         BNE   LR41A                                                            
         GOTO1 VRECUP,DMCB,(X'01',PLISREC),HLDDATEL,PLISREC+33                  
*                                                                               
*                                  MOVE PUBS FROM FUFFER TO IOAREA              
LR41A    EQU   *                                                                
         LA    R0,MAXELS                                                        
         LA    R7,PLISREC+33+PLDTL                                              
         CLI   LINCK,1                                                          
         BE    *+8                                                              
         LA    R7,PLISREC+33                                                    
         L     RF,VRECUP                                                        
         GOTO1 ,DMCB,(X'01',PLISREC)                                            
LR44     CLI   0(R8),0                                                          
         BE    LR45                                                             
         MVC   WORK(PLPBL),0(R8)                                                
         CLI   1(R8),PLPBL         TEST NEW ELEM                                
         BE    LR44B                                                            
         MVI   WORK+1,PLPBL        ENLARGE ELEM                                 
         XC    WORK+8(3),WORK+8    CLEAR FILTERS                                
LR44B    DS    0H                                                               
         GOTO1 (RF),(R1),,WORK,(R7)                                             
         LA    R7,PLPBL(R7)                                                     
         ZIC   RE,1(R8)                                                         
         AR    R8,RE                                                            
         BCT   R0,LR44                                                          
LR45     EQU   *                                                                
         MVI   0(R7),0             EOR                                          
         TM    RECSTAT,X'80'       TEST NEW                                     
         BZ    LR46                YES                                          
         LR    R1,R6                                                            
         BCTR  R1,R0                                                            
         MH    R1,=H'96'                                                        
         L     RF,AHLDWRKS                                                      
         LA    R1,0(RF,R1)                                                      
         MVC   DMWORK(96),0(R1)    RESTORE GETWORK                              
         BAS   RE,PUTREC                                                        
         B     LR48                                                             
LR46     XC    PLISKEY,PLISKEY                                                  
         XC    PLISCTL(6),PLISCTL                                               
         MVC   PLISKAGY,AGYALPHA                                                
         MVC   PLISKMED,LSTMED                                                  
         MVI   PLISKRCD,X'17'                                                   
         MVC   PLISKCLT,LSTCLT                                                  
         MVC   PLISKCOD,LSTLST                                                  
         MVC   PLISKLIN,LINCK                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(25),PLISKEY                                                  
         BAS   RE,ADDREC                                                        
LR48     EQU   *                                                                
         IC    R1,LINCK                                                         
         LA    R1,1(R1)                                                         
         STC   R1,LINCK                                                         
         BXLE  R3,R4,LR32                                                       
         LA    R2,LSTMEDH                                                       
         XC    LSTMSG,LSTMSG                                                    
         MVC   LSTMSG(L'OKMSG),OKMSG                                            
         FOUT  LSTMSGH                                                          
         XC    DSPFST(12),DSPFST   CLEAR DSPFST AND DSPLAST                     
*                                  AFTER SUCCESSFUL CHANGE                      
         B     EXIT                                                             
         DROP R3                                                                
         EJECT                                                                  
*                                  BUILD DATA DISPLAY FLDS                      
         SPACE 2                                                                
DATADISP NTR                                                                    
         SPACE 1                                                                
         BAS   RE,GETPUB                                                        
         L     R4,APUBIO                                                        
         USING PUBRECD,R4                                                       
         LA    R8,L'LSTSTRT+8(R2)  POINT TO DISP FLD                            
         LA    R8,L'LSTFILT+8(R8)                                               
         XC    8(L'LSTDATA,R8),8(R8)                                            
         CLI   LSTMED,C'N'                                                      
         BNE   DD2                                                              
*                                  NEWSPAPER DISP                               
         USING NDATAD,R8                                                        
         MVC   NDNAME,PUBNAME                                                   
         MVC   NDCITY,PUBCITY                                                   
         MVC   NDSTATE,PUBSTATE                                                 
         MVC   NDZONE,PUBZNAME                                                  
         B     DD4                                                              
*                                  MAGAZINE DISP                                
DD2      EQU   *                                                                
         USING MDATAD,R8                                                        
         MVC   MDNAME,PUBNAME                                                   
         MVC   MDZNAME,PUBZNAME                                                 
DD4      EQU   *                                                                
         FOUT  (R8)                                                             
         DROP R4                                                                
         DROP  R8                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                  PROCESS INPUT RECORDS                        
BUFFIN   NTR1                                                                   
         SPACE 2                                                                
         MVI   SAVKEY,0                                                         
         USING RECWKD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,LINCK                                                         
         LR    R2,R1                                                            
         BCTR  R2,R0                                                            
         MH    R2,=H'96'                                                        
         L     RF,AHLDWRKS                                                      
         LA    R2,0(RF,R2)                                                      
         MVC   0(96,R2),DMWORK     SAVE GETWORK TABLE                           
         LR    R2,R1                                                            
         MH    R2,=H'33'                                                        
         LA    R2,HLDKEYS-33(R2)                                                
         MVC   0(33,R2),IOAREA     SAVE 1ST 33 BYTES OF REC                     
         LR    R2,R1                                                            
         MH    R2,=H'31'                                                        
         LA    R2,HLDDIRS-31(R2)                                                
         MVC   0(31,R2),KEY        SAVE POINTER                                 
         TM    RECSTAT,X'40'                                                    
         BNZ   BFXIT               EXIT IF DELETE                               
         DROP R4                                                                
         LA    R9,IOAREA+33                                                     
         CLI   0(R9),X'10'         DATE ELEM                                    
         BNE   BF6                                                              
         USING PLISDTEL,R9                                                      
         MVC   HLDDATEL,PLISDTEL   SAVE DATE ELEM                               
         CLI   LSTACT,C'D'                                                      
         BNE   BF4                                                              
         MVC   LSTDESC,PLISDESC                                                 
         FOUT  LSTDESCH                                                         
BF4      SR    R0,R0                                                            
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
BF6      CLI   0(R9),X'20'         PUB EL                                       
         BE    BF8                                                              
         CLI   0(R9),0             EOR                                          
         BNE   BF4                                                              
BFXIT    DS    0H                                                               
         CLI   SAVKEY,0                                                         
         BE    BFXIT2                                                           
         MVC   KEY(64),SAVKEY                                                   
         BAS   RE,HIGH                                                          
BFXIT2   DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
         USING PLISPBEL,R9                                                      
BF8      LH    R1,PUBSIN                                                        
         LA    R1,1(R1)                                                         
         STH   R1,PUBSIN                                                        
         CLI   LSTACT,C'D'                                                      
         BNE   BF18                                                             
         OC    STRTNUM,STRTNUM                                                  
         BZ    BF12                                                             
         CLC   PUBSIN,STRTNUM                                                   
BF10     BNL   BF14                                                             
         B     BF4                                                              
BF12     CLC   PLISPUB(6),STRTPUB                                               
         BL    BF4                                                              
BF14     L     R4,ANXTOUT                                                       
         LA    R0,LSTDUMH                                                       
         CR    R4,R0                                                            
         BL    BF16                                                             
         MVI   MORESW,1                                                         
         B     BFXIT                                                            
BF16     EQU   *                                                                
         MVI   8(R4),X'40'                                                      
         MVC   9(L'LSTSTRT-1,R4),8(R4)                                          
         GOTO1 VPUBED,DMCB,PLISPUB,8(R4)                                        
         SPACE 1                                                                
         FOUT  (R4)                                                             
*                                                                               
         LA    R2,8+L'LSTSTRT(R4)  FILTERS                                      
         MVC   8(3,R2),=3C' '                                                   
         CLI   PLISPBEL+1,11                                                    
         BL    *+10                                                             
         MVC   8(3,R2),PLISFILT                                                 
         FOUT  (R2)                                                             
         MVC   DSPLAST,PLISPUB                                                  
         OC    DSPFST,DSPFST                                                    
         BNZ   *+10                                                             
         MVC   DSPFST,PLISPUB                                                   
         LR    R2,R4                                                            
         LA    R5,KEY                                                           
         USING PUBRECD,R5                                                       
*                                  READ FILE TO GET DISPLAY INFO                
         CLI   SAVKEY,0                                                         
         BNE   *+10                                                             
         MVC   SAVKEY,KEY          HOLD KEY AND KEYSAVE                         
         XC    KEY,KEY                                                          
         MVC   PUBKMED,LSTMED                                                   
         MVC   PUBKPUB(6),PLISPUB                                               
         MVC   PUBKAGY,AGYALPHA                                                 
         MVI   PUBKCOD,X'81'                                                    
         NI    DMINBTS,X'F7'       DO NO PASS DELETES                           
         BAS   RE,HIGHPUB                                                       
         B     *+8                                                              
BF16A    BAS   RE,SEQPUB                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BNE   BF17                                                             
         CLC   PUBKAGY,AGYALPHA                                                 
         BE    BF16C                                                            
         CLI   SVAGPROF+16,C'0'                                                 
         BE    BF17      DONT LOOK FOR SRDS                                     
         CLC   PUBKAGY,=C'ZZ'                                                   
         BNE   BF16A                                                            
BF16C    EQU   *                                                                
         CLI   PUBKCOD,X'81'       TEST REG REC                                 
         BNE   BF16A                                                            
         DROP  R5                                                               
         BAS   RE,DATADISP                                                      
BF16D    EQU   *                                                                
         OI    4(R4),X'20'         SET VAL                                      
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         IC    R0,0(R4)            PAST FILTER                                  
         AR    R4,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0               BUMP PAST DISPLAY FIELDS                     
         ST    R4,ANXTOUT                                                       
         B     BF4                                                              
*                                  PUB NOT FOUND ON DISPLAY                     
BF17     EQU   *                                                                
         LA    R8,L'LSTSTRT+8(R2)   POINT TO DISP FLD                           
         LA    R8,8+L'LSTFILT(R8)                                               
         MVC   8(17,R8),=C'**PUB NOT FOUND**'                                   
         FOUT  (R8)                                                             
         B     BF16D                                                            
BF18     CLI   LSTACT,C'A'                                                      
         BE    BF20                                                             
         CLC   PLISPUB(6),DSPFST                                                
         BL    BF20                                                             
         CLC   PLISPUB(6),DSPLAST                                               
         BNH   BF4                                                              
BF20     L     R4,ANXTBUF                                                       
         GOTO1 VRECUP,DMCB,(X'FF',PUBBUFF),(R9),(R4)                            
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         ST    R4,ANXTBUF                                                       
         B     BF4                                                              
         SPACE 2                                                                
         EJECT                                                                  
*****            PPGENEROL                                                      
*                                                                               
*                  INITIALISATION CODE                                          
         SPACE 3                                                                
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    R3,IOAREA                                                        
         ST    R3,AREC                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBDIR)                     
         SPACE 3                                                                
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBFILE)                    
         SPACE 3                                                                
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
OKMSG    DC    C'ACTION COMPLETE'                                               
*NOP*MORMSG   DC    C'TO CONTINUE DISPLAY, TYPE ''NEXT'' AND ENTER'             
MORMSG   DC    C'SHORTER FOR CSECT LENGTH'                                      
MAXBUFF  DC    AL2(MAXELS*MAXRECS*PLPBL+2)                                      
         SPACE 3                                                                
*                                  LIST OF ACTIONS                              
         SPACE 2                                                                
         CNOP  2,4                                                              
ACTLST   DC    H'8'                                                             
         DC    A(ACTLSTX)                                                       
         DC    CL8'ADD'                                                         
         DC    CL8'CHANGE'                                                      
         DC    CL8'DISPLAY'                                                     
ACTLSTX  EQU   *-1                                                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENOLD                                                       
         SPACE 2                                                                
         DS    1000C                                                            
LINCK    DS    CL1                                                              
DELSW    DS    CL1                                                              
MORESW   DS    CL1                                                              
HAVPUB   DS    CL1                                                              
PUBSIN   DS    H                                                                
ANXTOUT  DS    A                                                                
ANXTBUF  DS    A                                                                
VPUBVAL  DS    A                                                                
VPUBED   DS    A                                                                
AHLDWRKS DS    A                   A(HLDWRKS) - NEEDED BECAUSE WS               
*                                  TOO BIG FOR 1 REG                            
STRTNUM  DS    H                                                                
STRTPUB  DS    CL6                                                              
SAVKEY   DS    CL64                                                             
RECWK    DS    CL40                RECORD WORK AREA                             
HLDDATEL DS    CL27                HOLD DATE ELEM                               
HLDKEYS  DS    231C                33 X 7 KEYS                                  
HLDDIRS  DS    217C                31 X 7 DIR RECS                              
RELO     DS    A                                                                
         DS    0H                                                               
PUBBUFF  DS    6500C               BUFFER FOR PUBS                              
HLDWRKS  DS    672C                 HOLD ARES FOR 7 GETWORKS                    
*                                                                               
PUBIO    DS    4000C                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE T40AFFD                                                        
         SPACE 3                                                                
DSPFST   DS    CL6                                                              
DSPLAST  DS    CL6                                                              
SVAGPROF DS    CL30                                                             
*                                                                               
PLISRECD DSECT                                                                  
       ++INCLUDE PLISREC                                                        
         SPACE 3                                                                
*                                                                               
PLISDTD  DSECT                                                                  
       ++INCLUDE PLISDTEL                                                       
PLDTL    EQU   *-PLISDTEL                                                       
         SPACE 3                                                                
*                                                                               
PLISPBD  DSECT                                                                  
       ++INCLUDE PLISPBEL                                                       
PLPBL    EQU   *-PLISPBEL                                                       
         SPACE 3                                                                
PAGYRECD DSECT                                                                  
*                                                                               
       ++INCLUDE PAGYREC                                                        
         SPACE 3                                                                
PCLTRECD DSECT                                                                  
*                                                                               
       ++INCLUDE PCLTREC                                                        
         SPACE 3                                                                
         SPACE 3                                                                
PUBRECD  DSECT                                                                  
*                                                                               
       ++INCLUDE PUBREC                                                         
*                                                                               
       ++INCLUDE PUBNAMEL                                                       
*                                  RECORD CONTROL DSECT                         
RECWKD   DSECT                                                                  
RECDSK   DS    F                   DISK ADDRESS                                 
RECSTAT  DS    X                   X'80' = PINTER THERE                         
*                                  X'40' = PINTER WAS DELETED                   
*                                  X'20' = RECORD NEEDS TO BE WRITTEN           
         DS    3X                  SPARE                                        
         SPACE 3                                                                
*                                  NEWSPAPER DISPLAY DSECT                      
NDATAD   DSECT                                                                  
         DS    CL8                                                              
NDATA    DS    0CL59                                                            
NDNAME   DS    CL20                                                             
         DS    CL1                                                              
NDCITY   DS    CL16                                                             
         DS    CL1                                                              
NDSTATE  DS    CL2                                                              
         DS    CL1                                                              
NDZONE   DS    CL18                                                             
*                                  MAGAZINE DISPLAY DSECT                       
MDATAD   DSECT                                                                  
         DS    CL8                                                              
MDATA    DS    0CL59                                                            
MDNAME   DS    CL20                                                             
         DS    CL1                                                              
MDZNAME  DS    CL20                                                             
         DS    CL18                                                             
*                                                                               
DELBTS   EQU   X'80'                                                            
UNDELBTS EQU   X'7F'                                                            
MAXELS   EQU   80                                                               
MAXRECS  EQU   7                                                                
*                                                                               
*                                  ERROR NUMBER EQUATES                         
*                                                                               
ACTERR   EQU   12                  INVALID ACTION                               
CLTERR   EQU   14                  INVALID CLIENT                               
CHNGERR  EQU   142                 CHANGE NOT PRECEDED BY DISPLAY               
LISTERR  EQU   25                  INVALID LIST CODE                            
MEDERR   EQU   13                  INVALID MEDIA                                
MISSERR  EQU   1                                                                
NXTERR   EQU   143                 NO PREVIOUS DISPLAY                          
OVFERR   EQU   144                                                              
PUBERR   EQU   18                  INVALID PUB CODE                             
PUBERR2  EQU   44                  PUB NOT FOUND                                
PUBERR3  EQU   125                 ADDING DUPLICATE PUB                         
PUBERR4  EQU   124                 NO PUBS FOUND                                
RECERR   EQU   53                  RECORD NOT FOUND                             
STRTERR  EQU   26                  INVALID START                                
*                                                                               
       ++INCLUDE FLDIND                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073T40A00    05/01/02'                                      
         END                                                                    
         SPACE 3                                                                
