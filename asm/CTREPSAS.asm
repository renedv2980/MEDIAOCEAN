*          DATA SET CTREPSAS   AT LEVEL 029 AS OF 10/05/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE CTREPSAA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE SQUASHER                                                               
***INCLUDE FATABOFF                                                             
*&&      SET   NOP=N                                                            
         TITLE 'CTFILE REPORTS FOR SAS'                                         
*************************************************************                   
*        COPY CTFILE TO FLAT FILE FOR SAS READS             *                   
*************************************************************                   
CTREPSAS CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CTSAS*,RA,R9,WORK=A(WORKC),CLEAR=YES               
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RD,SAVERD                                                        
*                                                                               
         L     R1,=A(IO-WORKD)     ADDRESS OUT OF RANGE WORK                    
         AR    R1,RC                                                            
         ST    R1,AIO                                                           
*                                                                               
         L     R1,=A(IO2-WORKD)    ADDRESS OUT OF RANGE WORK                    
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
*                                                                               
         LA    R1,1                INIT RECORD SEQUENCE NUMBER                  
         ST    R1,SEQUENCE                                                      
*                                                                               
MAIN     BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             OPEN FILES ECT                               
         BAS   RE,GETSA            FILL SECURITY AGENCY TABLE                   
         BAS   RE,GETPRINC         FILL PRINCIPLE ID TABLE                      
         BAS   RE,BOUNDS           SET HIGH LOW KEYS FOR REPORT                 
*                                                                               
MAIN010  BAS   RE,GETNEXT          READ FILE                                    
         BAS   RE,FILTER           FILTER OUT UNWANTED RECORDS                  
         BNE   MAIN010                                                          
*                                                                               
         BAS   RE,PUTNEXT          PUT NEW RECORDS                              
         B     MAIN010                                                          
*                                                                               
TAPEEND  L     RD,SAVERD           RESTORE RD POSITION                          
*                                                                               
MAIN990  BAS   RE,CLOSE            CLOSE FILES ECT                              
*                                                                               
XBASE    XBASE ,                   PROG EXIT                                    
*                                                                               
EXITN    LTR   RB,RB               EXIT CC=NEQ                                  
         B     EXIT                                                             
EXITY    CR    RB,RB               EXIT CC=EQU                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISATION / OPEN FILES                        *                   
*************************************************************                   
INIT     NTR1                                                                   
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         MVI   CTINIT,C'N'                                                      
*                                                                               
INIT010  LA    R3,IOAREA                                                        
         GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
         LR    R1,R3                                                            
         BAS   RE,VALCARD          DO CARD VALIDATION                           
         BE    INIT010                                                          
         B     TAPEEND             EXIT IF ERRORS                               
*                                                                               
         USING SSBD,R1                                                          
INIT020  L     R1,=V(SSB)                                                       
         OI    SSOFLAG1,SSOFXCPY   TURN OFF SSB COPY                            
         MVC   SSODSPAC,DSPACE                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,=V(DDSIO)        SET UP DDSIO                                 
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
         GOTO1 =V(DMSYSFIL),ATABS                                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,CONTROL,CTFILES,IOAREA                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT990  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD TABLE OF SECURITY AGENCIES                   *                   
*************************************************************                   
GETSA    NTR1                                                                   
         LARL  R3,SATABLE          TABLE OF AG TO SA                            
         L     R2,AIO                                                           
         USING CT5REC,R2           READ RECORDS                                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CT5KEY,CT5KEY,IOWORK              
         TM    8(R1),X'80'                                                      
         BO    GETSAX                                                           
         CLI   8(R1),0                                                          
         BE    GETSANX1                                                         
         DC    H'00'                                                            
*                                                                               
GETSANXT L     R2,AIO                                                           
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,CT5KEY,CT5KEY,IOWORK              
         TM    8(R1),X'80'                                                      
         BO    GETSAX                                                           
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   GETSAX                                                           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
GETSANX1 GOTO1 VHELLO,PARM,(C'G',CTFILE),('CTSEAELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   GETSANXT            NO NAME ELEMENT                              
         L     R5,12(R1)                                                        
         MVC   0(2,R3),CT5KALPH    COPY AG/SA                                   
         MVC   2(2,R3),2(R5)                                                    
         LA    R3,4(R3)            NEXT                                         
         LARL  R1,SATABLEX                                                      
         CR    R3,R1                                                            
         BL    GETSANXT                                                         
         DC    H'0'                                                             
*                                                                               
GETSAX   XC    0(4,R3),0(R3)       EOT                                          
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD TABLE OF PRINCIPLE ID                        *                   
*************************************************************                   
GETPRINC NTR1                                                                   
         LARL  R3,PRINCIPL         TABLE OF AG TO PRINCIPLE                     
*                                                                               
         L     R2,AIO                                                           
         USING CT5REC,R2           READ RECORDS                                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CT5KEY,CT5KEY,IOWORK              
         TM    8(R1),X'80'                                                      
         BO    GETPRX                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GETPRIX1                                                         
*                                                                               
GETPRINX L     R2,AIO                                                           
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,CT5KEY,CT5KEY,IOWORK              
         TM    8(R1),X'80'                                                      
         BO    GETPRX                                                           
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   GETPRX                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETPRIX1 GOTO1 VHELLO,PARM,(C'G',CTFILE),('CTDSCELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   GETPRINX            NO NAME ELEMENT                              
         L     R5,12(R1)                                                        
         MVC   0(2,R3),CT5KALPH    COPY AG/PRINCIPLE NUM                        
         MVC   2(2,R3),2(R5)                                                    
         LA    R3,12(R3)                                                        
         LARL  R1,PRINCIPX                                                      
         CR    R3,R1                                                            
         BL    GETPRINX                                                         
         DC    H'0'                NEED BIGGER TABLE                            
*                                                                               
GETPRX   XC    0(4,R3),0(R3)       EOT                                          
*                                                                               
         LARL  R3,PRINCIPL         TABLE OF AG TO PRINCIPLE                     
GETPRX1  OC    0(2,R3),0(R3)                                                    
         JZ    GETPRX9                                                          
*                                                                               
         MVC   HEXID,=C'ID=XXXX   '                                             
         GOTO1 =V(HEXOUT),DMCB,(0,2(R3)),HEXID+3,2                              
*                                                                               
         L     R2,AIO                                                           
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,CTIKTYPQ                                                  
         MVC   CTIKNUM,2(R3)                                                    
         GOTO1 =V(DATAMGR),DMCB,DMREAD,CTFILE,CTIKEY,CTIKEY,IOWORK              
         CLI   8(R1),0                                                          
         BE    GETPRX2                                                          
         MVC   2(10,R3),HEXID                                                   
         J     GETPRX5                                                          
*                                                                               
GETPRX2  GOTO1 VHELLO,PARM,(C'G',CTFILE),('CTDSCELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         JE    GETPRX3             NO NAME ELEMENT                              
         MVC   2(10,R3),HEXID                                                   
         J     GETPRX5                                                          
*                                                                               
GETPRX3  L     R5,12(R1)                                                        
         MVC   2(10,R3),2(R5)      COPY NAME                                    
GETPRX5  LA    R3,12(R3)                                                        
         J     GETPRX1                                                          
*                                                                               
GETPRX9  J     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        SET HIGH LOW KEY FOR REPORT                        *                   
*************************************************************                   
BOUNDS   NTR1                                                                   
         MVC   LOWKEY,ZEROS                                                     
         MVC   HIGHKEY,FFS                                                      
*                                                                               
BOUND010 CLI   REPORT,C'P'         PERSON REPORT                                
         BNE   BOUND020                                                         
         MVC   LOWKEY(2),=X'C604'  F04 0000                                     
         MVC   HIGHKEY(2),=X'C604'  F04 FFFF                                    
         B     BOUNDSX                                                          
*                                                                               
BOUND020 CLI   REPORT,C'U'         USERID REPORT                                
         BNE   BOUND030                                                         
         MVC   LOWKEY(1),=C'I'     I 0000                                       
         MVC   HIGHKEY(1),=C'I'    I 00/FFFF                                    
         MVC   HIGHKEY+1(24),ZEROS                                              
         MVC   HIGHKEY+23(2),FFS                                                
         B     BOUNDSX                                                          
*                                                                               
BOUND030 EQU   *                                                                
*                                                                               
BOUNDSX  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE FILES                                        *                   
*************************************************************                   
CLOSE    NTR1                                                                   
*                                                                               
         CLOSE TAPEOUT             CLOSE TAPES AND SORT                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMCLSE,CONTROL,0,IOAREA                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
         B     EXITY                                                            
         EJECT                                                                  
************************************************************                    
*        READ NEXT RECORD FROM CTFILE                      *                    
************************************************************                    
GETNEXT  NTR1                                                                   
*                                                                               
         CLI   CTINIT,C'N'                                                      
         BNE   GETNEXT1                                                         
*                                                                               
GETNXDMP L     R2,AIO                                                           
         USING CTIREC,R2           READ RECORDS                                 
         XC    CTIKEY,CTIKEY                                                    
         MVC   CTIKEY,LOWKEY                                                    
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY,IOWORK              
         TM    8(R1),X'80'                                                      
         BO    TAPEEND                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CTINIT,C'Y'                                                      
         XC    GETNXDMP(4),GETNXDMP                                             
         B     GETNEXTX                                                         
*                                                                               
GETNEXT1 L     R2,AIO                                                           
         TM    READFLAG,X'80'      CHECK NEED TO RESTORE SEQ                    
         BO    GETNEXT2                                                         
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,CTFILE,CTIKEY,CTIKEY,IOWORK              
         TM    8(R1),X'80'                                                      
         BO    TAPEEND                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   OLDKEY,CTIKEY                                                    
         B     GETNEXTX                                                         
*                                                                               
GETNEXT2 MVI   READFLAG,0                                                       
         MVC   CTIKEY,OLDKEY                                                    
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY,IOWORK              
         MVC   OLDKEY,CTIKEY                                                    
         B     GETNEXT1                                                         
*                                                                               
GETNEXTX CLC   0(25,R2),HIGHKEY    FORCE EOF ON HIGHKEY                         
         BL    EXITY                                                            
         BNL   TAPEEND                                                          
         EJECT                                                                  
************************************************************                    
*        PUT CSV RECORD TO TAPEOUT                         *                    
************************************************************                    
PUTNEXT  NTR1                                                                   
         CLI   REPORT,C'P'         PUT PERSON RECORDS                           
         BNE   PUTN010                                                          
         BRAS  RE,PUTPERS                                                       
         B     PUTNX                                                            
*                                                                               
PUTN010  CLI   REPORT,C'U'         PUT USERID RECORDS                           
         BNE   PUTN020                                                          
         BRAS  RE,PUTUSER                                                       
         B     PUTNX                                                            
*                                                                               
PUTN020  EQU   *                                                                
*                                                                               
PUTNX    B     EXIT                                                             
         EJECT                                                                  
************************************************************                    
*        PUT PERSON RECORD DETAIL TO TAPEOUT               *                    
************************************************************                    
PUTPERS  NTR1                                                                   
         L     R2,AIO                                                           
         USING SAPEREC,R2                                                       
         L     R3,AIO2                                                          
         USING PERLINED,R3                                                      
         CLC   OLDPID,SAPEPID                                                   
         JE    PUTPERSX                                                         
*                                                                               
         MVC   OLDPID,SAPEPID                                                   
         MVC   PERLALL(200),SPACES                                              
         MVC   PERLAGY,SAPEAGY     AGU ID                                       
         MVC   PERLPID,SAPEPID     PERSON ID                                    
*                                                                               
         LA    R1,SAPEDATA                                                      
         USING SAPWDD,R1                                                        
PUTPER01 CLI   0(R1),SAPWDELQ      PASSWORD NUMBER ELEMENT                      
         BE    PUTPER02                                                         
         XR    R0,R0                                                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     PUTPER01                                                         
*                                                                               
PUTPER02 XC    FULL,FULL                                                        
         MVC   FULL+2(2),SAPWDNUM                                               
         DROP  R1                                                               
         DROP  R2                                                               
*                                                                               
         EDIT  (B4,FULL),(6,PERLNUM),ALIGN=LEFT                                 
*                                                                               
         CLC   PERLALL,OLDUSER     DON'T PRINT OLD PASSWORDS                    
         BE    EXIT                                                             
         MVC   OLDUSER,PERLALL     KEEP FOR NEXT RECORD                         
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SANAMELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   PUTPER99            NO NAME ELEMENT                              
         L     R5,12(R1)                                                        
         USING SANAMD,R5                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SANAMLN        WHOLE ELEMENT LEN                            
         AR    R1,R5                                                            
         ST    R1,FULL             SAVE A(END)                                  
         MVC   PERLNAME,SPACES                                                  
*                                                                               
         XR    R0,R0               ZERO NAME COUNTER                            
         LA    RF,SANAMES                                                       
         LA    RE,WORK                                                          
         MVC   WORK(80),SPACES                                                  
CHKP215  AHI   R0,1                COUNT WHICH NAME WE ARE ON                   
         XR    R1,R1                                                            
         ICM   R1,1,0(RF)                                                       
         SHI   R1,1                                                             
*                                                                               
         CHI   R0,2                                                             
         BNE   CHKP216                                                          
         TM    SANAMIND,SANAMIMN   DON'T BOTHER WITH MIDDLE NAME                
         BNZ   CHKP217                                                          
*                                                                               
CHKP216  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),1(RF)       COPY NAME TO PRINT AREA                      
         LA    RE,2(RE,R1)                                                      
*                                                                               
CHKP217  LA    RF,2(RF,R1)                                                      
         C     RF,FULL                                                          
         BL    CHKP215                                                          
*                                                                               
         MVC   PERLNAME,WORK                                                    
         MVC   PERLEMAI,SPACES                                                  
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SAPEEELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   PUTPER99            NO NAME ELEMENT                              
         L     R5,12(R1)                                                        
         USING SAPEED,R5                                                        
*                                                                               
         MVI   BYTE,0                                                           
         SR    R1,R1               DO SOME EMAIL VALIDATION                     
         ICM   R1,1,SAPEELN                                                     
         AHI   R1,-2                                                            
         LA    RE,SAPEEID                                                       
PUTPER97 CLI   0(RE),C' '          NO SPACES ALLOWED                            
         JNE   *+12                                                             
         CLI   BYTE,C'@'           ONCE @ FOUND                                 
         JE    PUTPER99                                                         
         CLI   0(RE),C'@'          MUST HAVE AN @ IN IT                         
         JNE   *+8                                                              
         MVI   BYTE,C'@'                                                        
         LA    RE,1(RE)                                                         
         JCT   R1,PUTPER97                                                      
         CLI   BYTE,C'@'                                                        
         JNE   PUTPER99                                                         
*                                                                               
PUTPER98 SR    R1,R1                                                            
         ICM   R1,1,SAPEELN        WHOLE ELEMENT LEN                            
         AHI   R1,-3                                                            
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   PERLEMAI(0),SAPEEID                                              
*                                                                               
PUTPER99 PUT   TAPEOUT,(R3)                                                     
*                                                                               
PUTPERSX B     EXIT                                                             
         EJECT                                                                  
************************************************************                    
*        PUT USERID RECORD DETAIL TO TAPEOUT               *                    
************************************************************                    
PUTUSER  NTR1                                                                   
         L     R2,AIO                                                           
         USING CTIREC,R2                                                        
         L     R3,AIO2                                                          
         USING ULINED,R3                                                        
         MVC   ULIALL,SPACES                                                    
         MVC   ULISESYS(15*7),SLOTNULS                                          
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),CTIKNUM                                                
         OC    FULL,FULL                                                        
         BZ    PTUSRX                                                           
         EDIT  (B4,FULL),(6,ULINUM),ALIGN=LEFT                                  
*                                                                               
         LA    R1,CTIDATA                                                       
         USING CTDSCD,R1                                                        
PTUSR01  CLI   0(R1),CTDSCELQ      PASSWORD NUMBER ELEMENT                      
         BE    PTUSR03                                                          
         CLI   0(R1),CTSYSELQ      SYSTEM ELEMENT                               
         BE    PTUSR04                                                          
         CLI   0(R1),CTAGYELQ      AGENCY ELEMENT                               
         BE    PTUSR03A                                                         
         CLI   0(R1),CTDSTELQ      DESTINATION ELEMENT                          
         BE    PTUSR03Q                                                         
*                                                                               
PTUSR02  CLI   0(R1),0                                                          
         BE    PTUSRX                                                           
         XR    R0,R0                                                            
         ICM   R0,1,1(R1)                                                       
         BZ    PTUSRX                                                           
         AR    R1,R0                                                            
         B     PTUSR01                                                          
*                                                                               
PTUSR03  XR    RF,RF                                                            
         IC    RF,CTDSCLEN                                                      
         SHI   RF,3                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ULINAME(0),CTDSC                                                 
         CLI   ULINAME,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     PTUSR02                                                          
*                                                                               
PTUSR03A MVC   ULIAGY(2),CTAGYID-CTAGYD(R1)                                     
         MVI   ULISAGY,C'*'                                                     
*                                                                               
         LARL  RF,PRINCIPL                                                      
         LARL  RE,PRINCIPX                                                      
PTUSR03B CR    RF,RE                                                            
         JNL   PTUSR03X                                                         
         CLC   0(2,RF),ULIAGY                                                   
         JE    PTUSR03C                                                         
         LA    RF,12(RF)                                                        
         J     PTUSR03B                                                         
*                                                                               
PTUSR03C MVC   ULIPRINC,2(RF)                                                   
*                                                                               
PTUSR03X LARL  RF,SATABLE          TEST FOR SA                                  
         OC    0(4,RF),0(RF)                                                    
         BZ    PTUSR02                                                          
*                                                                               
PTUSR03Z CLC   ULIAGY(2),0(RF)     SAME AGY                                     
         BL    PTUSR02                                                          
         LA    RF,4(RF)            NEXT                                         
         BH    PTUSR03Z                                                         
         SHI   RF,2                EQ SO BACK UP                                
         MVC   ULISAGY(2),0(RF)    AND COPY OUT                                 
         B     PTUSR02                                                          
*                                                                               
PTUSR03Q MVC   ULIPOW(4),CTDSTPOW-CTDSTD(R1)                                    
         MVC   ULICOMP,CTDSTNAM-CTDSTD(R1)                                      
         B     PTUSR02                                                          
*                                                                               
         USING CTSYSD,R1                                                        
PTUSR04  LA    RF,SLOTTAB                                                       
PTUSR04A CLC   CTSYSNUM,0(RF)      FIND SYSTEM                                  
         BE    PTUSR04B                                                         
         LA    RF,2(RF)                                                         
         CLI   0(RF),0                                                          
         BNE   PTUSR04A                                                         
         DC    H'0'                                                             
*                                                                               
PTUSR04B LLC   R0,1(RF)            INDEX TO SLOT                                
         LR    RF,R3                                                            
         AR    RF,R0                                                            
*                                                                               
         LLC   R5,CTSYSSE                                                       
         LR    R6,R5               SAVE SE IN R6                                
         MHI   R5,DMSYLEN                                                       
         A     R5,ASYSTAB          A(DMSYSTABUS) OR A(DMSYSTABUK)               
         USING SYSTABD,R5                                                       
         CLI   DMSYOVNO,0          MUST HAVE OVERLAY NUMBER                     
         BNE   PTUSR05                                                          
*&&US*&& MVC   0(5,RF),=C'GAMES'                                                
*&&US*&& CHI   R6,X'0B'            GAMES                                        
*&&US*&& BE    PTUSR02                                                          
         MVC   0(7,RF),=C'UNKNOWN'                                              
         B     PTUSR02                                                          
*                                                                               
PTUSR05  MVC   0(5,RF),DMSYNAM     SPT/MED/PRT/ACC/ETC                          
*&&US                                                                           
         CLI   DMSYNAM1,C'N'       NET                                          
         BNE   PTUSR05A                                                         
         MVC   0(3,RF),=C'NET'     REPLACE SPT WITH NET                         
         MVC   3(2,RF),DMSYNAM1+1                                               
*                                                                               
PTUSR05A CLC   0(3,RF),=C'SPT'                                                  
         BNE   PTUSR05B                                                         
         MVC   0(4,RF),=C'SPOT'                                                 
         MVC   4(2,RF),DMSYNAM+3                                                
*                                                                               
PTUSR05B CLC   0(3,RF),=C'PRT'                                                  
         BNE   PTUSR05C                                                         
         MVC   0(4,RF),=C'PRNT'                                                 
         MVC   4(2,RF),DMSYNAM+3                                                
*                                                                               
PTUSR05C CLC   0(3,RF),=C'CON'                                                  
         BNE   PTUSR05D                                                         
         MVC   0(7,RF),=C'CONTROL'                                              
*&&                                                                             
         DROP  R5                                                               
*                                                                               
PTUSR05D L     RF,ASYSTABS         LIST OF SYSTABS                              
         L     RE,12(,RF)          SYSSTAB                                      
         LR    R5,R6               RESTORE SE#                                  
         AR    R5,RE               POINT TO SEE WHAT UPDATE SYSTEM              
         CLI   0(R5),X'FF'         MULTIPLE SYSTEMS?                            
         BE    PTUSR05M            YES, SET TO MIXED                            
*                                                                               
         USING FACITABD,RE                                                      
         LLC   RE,0(R5)            GET THE ADV/REP/TST/CSC NUMBER               
         MHI   RE,L'FACITAB                                                     
         A     RE,AFACIDTB         SHOULD POINT TO FACID ENTRY                  
         CLC   FACISN4,SPACES                                                   
         BNE   PTUSR05E                                                         
         MVC   ULIADV,=C'ERROR'                                                 
         B     PTUSR02                                                          
*                                                                               
PTUSR05E CLC   ULIADV,SPACES                                                    
         BE    PTUSR05X                                                         
         CLC   ULIADV(4),FACISN4                                                
         BE    PTUSR02                                                          
*                                                                               
PTUSR05M MVC   ULIADV,=C'MIXED'                                                 
         B     PTUSR02                                                          
*                                                                               
PTUSR05X MVC   ULIADV(4),FACISN4                                                
         B     PTUSR02                                                          
         DROP  RE                                                               
         DROP  R1,R2                                                            
*                                                                               
PTUSRX   PUT   TAPEOUT,(R3)                                                     
         B     EXIT                                                             
*&&UK                                                                           
SLOTTAB  DC    AL1(4),AL1(30)      MEDIA                                        
         DC    AL1(5),AL1(37)      MPL                                          
         DC    AL1(6),AL1(44)      ACCOUNT                                      
         DC    AL1(7),AL1(51)      FEE                                          
         DC    AL1(9),AL1(58)      MBASE                                        
         DC    AL1(10),AL1(65)     CONTROL                                      
         DC    AL1(11),AL1(72)     GAMES                                        
         DC    AL1(12),AL1(79)     CPP                                          
         DC    AL1(14),AL1(86)     PER                                          
         DC    AL1(0)                                                           
*&&                                                                             
*&&US                                                                           
SLOTTAB  DC    AL1(2),AL1(30)      SPOT                                         
         DC    AL1(3),AL1(37)      NET                                          
         DC    AL1(4),AL1(44)      PRINT                                        
         DC    AL1(5),AL1(51)      MPL                                          
         DC    AL1(6),AL1(58)      ACC                                          
         DC    AL1(7),AL1(65)      TAL                                          
         DC    AL1(8),AL1(72)      REP                                          
         DC    AL1(9),AL1(79)      MBASE                                        
         DC    AL1(10),AL1(86)     CONTROL                                      
         DC    AL1(11),AL1(93)     GAMES                                        
         DC    AL1(12),AL1(100)    DEMO                                         
         DC    AL1(13),AL1(107)    STRAFFIC                                     
         DC    AL1(14),AL1(114)    PER                                          
         DC    AL1(0)                                                           
*&&                                                                             
SLOTNULS DC    C'*      *      *      *      *      *      *      '             
         DC    C'*      *      *      *      *      *      *      *'            
         DC    C'      '                                                        
         EJECT                                                                  
*************************************************************                   
*        FILTER RECORDS                                     *                   
*************************************************************                   
FILTER   NTR1                                                                   
*                                                                               
         L     R2,AIO                                                           
         CLI   REPORT,C'P'         PERSON REPORT                                
         BNE   *+14                                                             
         CLC   0(2,R2),=X'C604'    EXTRACT F04                                  
         BE    FILTERY                                                          
*                                                                               
         CLI   REPORT,C'U'         USERID REPORT                                
         BNE   *+12                                                             
         CLI   0(R2),C'I'          EXTRACT I                                    
         BE    FILTERY                                                          
*                                                                               
         B     FILTERX                                                          
*                                                                               
FILTERY  B     EXITY                                                            
FILTERX  B     EXITN                                                            
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARD CONSTANTS                           *                   
*************************************************************                   
DDSIO    DC     C'DDSIO   '                                                     
DSPACE   DC     C' '                                                            
         EJECT                                                                  
*************************************************************                   
*        VALIDATE PARAMETER CARDS                           *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*                                                                               
CARDTAB  DS    0F                                                               
         DC    C'REPORT ',AL1(5,0),X'C000',AL3(VALREP)                          
         DC    C'DSPACE ',AL1(5,0),X'0000',AL3(DSPACE)                          
         DC    C'DDSIO  ',AL1(4,8),X'0000',AL3(DDSIO)                           
         DC    X'0000'          EXTEND                                          
                                                                                
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC090                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC090                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC090                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC090                                                          
*                                                                               
VALC080  CLI   8(R4),0             DONT CARE                                    
         BE    *+12                                                             
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC090  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC090                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRSYS  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
CERRPRG  LA    R1,=C'INVALID PROGRAM '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SESYS   '                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
************************************************************                    
*        VALIDATE REPORT TYPE                              *                    
************************************************************                    
VALREP   NTR1                                                                   
*                                                                               
         LA    R1,REPTAB           VALIDATE REPORT FIELD                        
VALREP1  MVC   REPORT,7(R1)                                                     
         CLC   0(1,R1),0(R2)       1 CHR OK                                     
         BE    VALREPX                                                          
         CLI   REPORT,X'FF'        EOT                                          
         BE    VALREPX                                                          
         LA    R1,8(R1)                                                         
         B     VALREP1                                                          
*                                                                               
VALREPX  B     EXIT                                                             
         EJECT                                                                  
       ++INCLUDE DDVALNUM                                                       
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
         EJECT                                                                  
***************************************************                             
*        EDIT TUS IN FULL TO WORK HH:MM:SS.SS     *                             
***************************************************                             
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         EDIT  (RE),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        WAIT ROUTINE                                       *                   
*************************************************************                   
WAIT     ST    RE,SAVERE                                                        
         SR    R0,R0                                                            
         M     R0,=F'38400'        * 38400 FOR TUS                              
         ST    R1,FULL                                                          
         STIMER WAIT,TUINTVL=FULL  WAIT 10 SECS THEN TRY AGAIN                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'1'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT TITLE                                        *                   
*************************************************************                   
TITLE    DC    CL166' '                                                         
         ORG   TITLE                                                            
         DC    C'1',X'40'                                                       
         DC    C'                                                  '            
         DC    C'                                                  '            
         DC    C'                    '                                          
         ORG                                                                    
TITLE1   DC    CL166' '                                                         
         ORG   TITLE1                                                           
         DC    C'1',X'40'                                                       
         DC    C'-----------------------------------------------------'         
         DC    C'------------------ PARAMETER CARDS ------------------'         
         DC    C'-----------------------------------------------------'         
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        DCBS                                               *                   
*************************************************************                   
*                                                                               
*        LRECL=(166) USE CHARS=(BX15)                                           
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FB,LRECL=200,BUFNO=2                                       
         EJECT                                                                  
*************************************************************                   
*        TABLES                                             *                   
*************************************************************                   
REPTAB   DC    CL7'PERSON ',C'P'                                                
         DC    CL7'USERID ',C'U'                                                
         DC    CL7'XXXXXX ',X'FF'                                               
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
VSEL     DC    V(SELIST)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHELLO   DC    V(HELLO)                                                         
ATABS    DC    6A(0)                                                            
         ORG   ATABS                                                            
ASYSTAB  DS    A                                                                
AFILTAB  DS    A                                                                
ASYSTABS DS    A       A(A(SYSATAB),A(SYSFTAB),A(SYSITAB),A(SYSSTAB))           
AFACIDTB DS    A                                                                
         ORG                                                                    
MAXLINE  DC    P'60'                                                            
SPACES   DC    CL200' '                                                         
ZEROS    DC    32X'00'                                                          
FFS      DC    32X'FF'                                                          
DOTS     DC    20C'.'                                                           
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
DMUNLK   DC    CL8'DMUNLK'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
ADDREC   DC    CL8'ADDREC'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
*                                                                               
CONTROL  DC    CL8'CONTROL'                                                     
GENDIR   DC    CL8'GENDIR'                                                      
GENFIL   DC    CL8'GENFIL'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
CTFILES  DC    CL8'NCTFILE '                                                    
         DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFIL '                                                    
         DC    CL8'NCTRCVR '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         LTORG                                                                  
       ++INCLUDE FALANGTAB                                                      
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
*                                                                               
*********************************************************************           
* SSB AND ULT. ENTRY POINTS FOR DMDMGRL (CONNECTION TO DDSIO)                   
*********************************************************************           
                                                                                
         DS    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DS    0F                                                               
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENED SSB                              
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSNRCV)       SET NO RECOVERY                              
         ORG   SSB+(SSODSPAC-SSBD)                                              
         DC    C' '                                                             
         ORG   SSB+(SSOASSB-SSBD)                                               
         DC    A(SSB)                                                           
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'0A'               CONTROL                                      
         ORG                                                                    
*                                                                               
SATABLE  DC    1400CL4'    '                                                    
SATABLEX DC    XL4'00'                                                          
*                                                                               
PRINCIPL DC    1600CL12'            '                                           
PRINCIPX DC    12X'00'                                                          
*                                                                               
WORKC    DS    (64*K)X                WORKING STORAGE POOL                      
         EJECT                                                                  
*************************************************************                   
*        DSECT TO COVER WORKING STORAGE                     *                   
*************************************************************                   
K        EQU   1024                                                             
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
CARDEND  DS    A                                                                
ARECEND  DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
READFLAG DS    X                                                                
*                                                                               
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
WORK     DS    CL255                                                            
WORK1    DS    XL64                                                             
IOW      DS    12D                                                              
DA       DS    F                                                                
*                                                                               
AIO      DS    A                                                                
AIO2     DS    A                                                                
ACHANGE  DS    A                                                                
*                                                                               
LOWKEY   DS    CL25                                                             
HIGHKEY  DS    CL25                                                             
OLDKEY   DS    CL25                                                             
*                                                                               
OLDPID   DS    CL8                                                              
OLDUSER  DS    CL60                                                             
HEXID    DS    CL10                                                             
*                                                                               
SEQUENCE DS    F                                                                
*                                                                               
REPORT   DS    C                                                                
TYPE2    DS    C                   SAVE 2ND REC TYPE                            
*                                                                               
CTINIT   DS    C                                                                
*                                                                               
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
PLINE    DS    CL200                                                            
*                                                                               
IOWORK   DS    XL64                                                             
*                                                                               
GTSYS    DS    X                   OV SYSTEM                                    
GTSYSSE  DS    X                   SE SYSTEM                                    
GTSYSL   DS    X                   LEN FOR OVSYS NAME SEARCH                    
GTSYSN   DS    CL7                 SYS NAME                                     
GTPROGL  DS    X                   LEN FOR PROG NAME SEARCH                     
GTPROGN  DS    CL7                 PROG NAME                                    
GTPROG   DS    X                   PROG NUMBER                                  
GTLANGN  DS    CL3                 LANGUAGE NAME                                
GTLANG   DS    X                   LANGUAGE CODE                                
GTAPGMS  DS    A                   A(PRGMS LIST FOR SYSTEM)                     
GTADV    DS    X                   ADV NUMBER                                   
GTADVN   DS    CL4                 ADV 4CHR SYSTEM NAME                         
GTADVX   DS    CL4                 ADV 3CHR SYSTEM NAME + AOR                   
*                                                                               
IOL      DS    XL4                                                              
IOAREA   DS    4096C                                                            
*                                                                               
         DS    F                                                                
IOL2     DS    XL4                                                              
IO2      DS    8192C                                                            
*                                                                               
IO       DS    4096C                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
************************************************************                    
*        RECOVER DSECTS                                    *                    
************************************************************                    
RECOVERD DSECT                                                                  
RCVRLEN  DS    XL2                                                              
         DS    XL2                                                              
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
         EJECT                                                                  
************************************************************                    
*        OTHER DSECTS                                      *                    
************************************************************                    
* PRINT PERSON INFO DATA                                                        
************************************************************                    
PERLINED DSECT                                                                  
PERLALL  DS    0CL132                                                           
PERLAGY  DS    CL2                                                              
         DS    CL1                                                              
PERLPID  DS    CL8                                                              
         DS    CL1                                                              
PERLNAME DS    CL35                                                             
         DS    CL1                                                              
PERLNUM  DS    CL12                                                             
         DS    CL1                                                              
PERLEMAI DS    CL50                                                             
PERLILNQ EQU   *-PERLINED                                                       
                                                                                
************************************************************                    
* PRINT USER LINE                                          *                    
************************************************************                    
ULINED   DSECT                                                                  
ULIALL   DS    0CL200                                                           
ULINUM   DS    CL7                                                              
ULINAME  DS    CL10                                                             
ULIAGY   DS    CL3                                                              
ULISAGY  DS    CL3                                                              
ULIPOW   DS    CL7                                                              
ULISESYS DS    15CL7                                                            
ULIADV   DS    CL5                                                              
         DS    CL14                                                             
ULICOMP  DS    CL33                                                             
         DS    CL1                                                              
ULIPRINC DS    CL10                                                             
         DS    CL4                                                              
         DS    CL14                                                             
ULICOLNQ EQU   *-ULINED                                                         
         EJECT                                                                  
**********************************************************************          
* USING PRINT OFF / PRINT ON SO WE CAN SEE THE LINE NUMBERS FOR ERRORS          
**********************************************************************          
         PRINT ON                                                               
*FASYSLSTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*SEACSFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*DMSYSTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSYSTABD                                                      
         PRINT ON                                                               
*DMFILTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
*DMRCVREXT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMRCVREXT                                                      
         PRINT ON                                                               
*FAD                                                                            
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029CTREPSAS  10/05/15'                                      
         END                                                                    
