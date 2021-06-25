*          DATA SET FADUMPS    AT LEVEL 013 AS OF 05/01/02                      
*PHASE DUMP,*                                                                   
*INCLUDE DMDADDS                                                                
*INCLUDE DMDYNDD                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DUMPOUT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'PRINT FACPAK MINI/MAXI DUMPS'                                   
         PRINT NOGEN                                                            
DUMP     CSECT                                                                  
         NBASE 0,**DUMP**,RA,WORK=A(DUMPWORK)                                   
*&&DO                                                                           
DUMP1    L     R1,20               SAVE DOS COMRG ADDRESS                       
         SVC   33                                                               
         ST    R1,ACMRG                                                         
         MVC   UPSI,23(R1)                                                      
DUMP1X   DS    0H                                                               
*&&                                                                             
*&&OS                                                                           
DUMP1    ST    R1,ACMRG            SAVE MVS PARAM ADDRESS                       
         L     R1,0(R1)                                                         
         LH    R2,0(R1)            R2=L'PARM DATA                               
         LTR   R2,R2                                                            
         BZ    DUMP1X                                                           
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         LA    RF,UPSITAB                                                       
DUMP1A   CLI   0(R1),C'0'                                                       
         BE    DUMP1B                                                           
         CLI   0(R1),C'1'                                                       
         BNE   DUMP1X                                                           
         OC    UPSI,0(RF)                                                       
DUMP1B   LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,DUMP1A                                                        
         B     DUMP1X                                                           
UPSITAB  DC    X'8040201008040201'                                              
DUMP1X   DS    0H                                                               
*&&                                                                             
*                                                                               
DUMP2    L     R9,VCPRINT          R9=A(PRINT CSECT)                            
         USING DPRINT,R9                                                        
         MVC   MID1(6),=C'GR 0-7'                                               
         MVC   MID2(6),=C'GR 8-F'                                               
         B     OPEN                                                             
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
OPEN     XC    P1(24),P1           OPEN DMPFILE AND GET DEVICE DATA             
         LA    R2,DMPFILE                                                       
         ST    R2,P4                                                            
         GOTO1 DADDS,P1,V(DAOPEN)                                               
         L     RF,BLKSIZE                                                       
         GOTO1 DADDS,P1,V(DARPT),,(RF)                                          
         LH    R2,P3+2                                                          
         LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R2,RECTRK           SAVE RECORDS PER TRACK                       
         L     R3,P2                                                            
         LH    R3,2(R3)                                                         
         ST    R3,TRKCYL           SAVE TRKS PER CYL                            
*                                                                               
         MVC   ADDR,=X'FFFFFFFF'                                                
         GOTO1 DADDS,P1,V(DATRNS),,,,ADDR,DUB                                   
         SR    RF,RF                                                            
         ICM   RF,3,DUB            RF=NUM OF TRKS IN DMPFILE                    
         ST    RF,TOTTRKS                                                       
         SR    RE,RE                                                            
         DR    RE,R3               RF=NUM OF CYLS IN DMPFILE                    
         ST    RF,TOTCYLS                                                       
*                                                                               
         LA    R8,DMPHD            READ FIRST DUMP HEADER RECORD                
         USING DMPHDRD,R8                                                       
         MVC   ADDR,=X'00010100'                                                
         GOTO1 DADDS,P1,V(RDID),(R8),,,ADDR                                     
         OC    P3(2),P3                                                         
         BNZ   DMPERR              NO DUMPS ON FILE                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,DMPCYL           R1=NUM OF CYLS/DUMP                          
         ST    R1,NUMCYLS                                                       
         SR    RE,RE                                                            
         L     RF,TOTCYLS                                                       
         DR    RE,R1                                                            
         STC   RF,MAXDUMP          SAVE MAX NUMBER OF DUMPS                     
         M     R0,TRKCYL                                                        
         ST    R1,NUMTRKS          SAVE TRACKS/DUMP                             
         EJECT                                                                  
INPUT    TM    UPSI,X'80'          UPSI=1 IS INPUT FROM CARDS                   
         BZ    INP2                                                             
*                                                                               
INP0     GOTO1 VCARDS,DMCB,SAVEAREA,=C'RE00'                                    
         CLC   SAVEAREA(2),=C'/*'                                               
         BE    EXIT                                                             
         MVI   DUMPTYPE,0                                                       
         CLC   SAVEAREA(2),=C'MAXI'                                             
         BE    *+18                                                             
         MVI   DUMPTYPE,1                                                       
         CLC   SAVEAREA(2),=C'MINI'                                             
         BNE   INP1                                                             
         TM    SAVEAREA+4,X'F0'                                                 
         BNO   INP1                                                             
         PACK  DUB,SAVEAREA+4(1)                                                
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BZ    INP1                                                             
         STC   R4,REQDUMP                                                       
         CLC   REQDUMP,MAXDUMP                                                  
         BNH   DMP                                                              
INP1     GOTO1 VLOGIO,DMCB,1,=C'*DUMP* ERROR ON PARAMETER CARD'                 
         B     EXIT                                                             
*                                                                               
INP2     GOTO1 VLOGIO,DMCB,1,=C'*DUMP* MINI OR MAXI DUMP ?'                     
         MVC   MSGAREA,SPACES                                                   
         GOTO1 (RF),(R1),0,(4,MSGAREA)                                          
         CLC   MSGAREA(3),=C'EOJ'                                               
         BE    EXIT                                                             
         MVI   DUMPTYPE,0                                                       
         CLC   MSGAREA(4),=C'MAXI'                                              
         BE    INP4                                                             
         MVI   DUMPTYPE,1                                                       
         CLC   MSGAREA(4),=C'MINI'                                              
         BE    INP4                                                             
         GOTO1 (RF),(R1),1,=C'*DUMP* INVALID REPLY'                             
         B     INP2                                                             
INP4     GOTO1 (RF),(R1),1,=C'*DUMP* ENTER ONE DIGIT DUMP NUMBER'               
         MVC   MSGAREA,SPACES                                                   
         GOTO1 (RF),(R1),0,(1,MSGAREA)                                          
         TM    MSGAREA,X'F0'                                                    
         BO    INP8                                                             
INP6     GOTO1 (RF),(R1),1,=C'*DUMP* INVALID REPLY'                             
         B     INP4                                                             
INP8     PACK  DUB,MSGAREA(1)                                                   
         CVB   R4,DUB                                                           
         LTR   R4,R4                                                            
         BZ    INP6                                                             
         STC   R4,REQDUMP                                                       
         CLC   REQDUMP,MAXDUMP     REQUESTED GR MAXIMUM                         
         BH    INP6                                                             
         EJECT                                                                  
DMP      MVC   ADDR,=X'00010100'   READ REQUESTED DUMP HEADER RECORD            
         SR    RF,RF                                                            
         IC    RF,REQDUMP                                                       
         BCTR  RF,0                                                             
         M     RE,NUMTRKS                                                       
         LA    RF,1(RF)            RF=TTTT FOR DUMP HEADER REC                  
         STH   RF,ADDR                                                          
*                                                                               
         MVC   ADDRSTR,ADDR        SAVE START DUMP ADDR                         
         A     RF,NUMTRKS                                                       
         BCTR  RF,0                                                             
         STH   RF,ADDREND          SAVE END DUMP ADDR                           
*                                                                               
         GOTO1 DADDS,P1,V(RDID),(R8),,,ADDR                                     
         OC    P3(2),P3                                                         
         BNZ   DMPERR                                                           
         LA    RF,DMPRC            SET I/O AREA ADDRESS                         
         ST    RF,P2                                                            
         B     DMP2                                                             
*                                                                               
DMPERR   B     DUMPXT         <================== NOP ERROR LOGIC               
******DMPERR   GOTO1 VLOGIO,DMCB,1,=C'*DUMP* DISK ERROR ON DUMP FILE'           
         DC    H'0'                                                             
*                                  FORMAT HEADLINES/MIDLINES ETC.               
DMP2     MVC   WORK(36),DMPREGS+28                                              
         MVC   WORK+36(28),DMPREGS                                              
         LA    R2,WORK                                                          
         LA    R3,MID1+9                                                        
         LR    R6,R3                                                            
         LA    R5,2                                                             
*                                                                               
DMP4     LA    R7,2                                                             
DMP6     LA    R4,4                                                             
*                                                                               
DMP8     GOTO1 VHEXOUT,DMCB,(R2),(R3),4,=C'TOG'                                 
         LA    R2,4(R2)                                                         
         LA    R3,09(R3)                                                        
         BCT   R4,DMP8                                                          
         LA    R3,2(R3)                                                         
         BCT   R7,DMP6                                                          
         LA    R3,132(R6)                                                       
         BCT   R5,DMP4                                                          
         MVC   SUB1,SPACES                                                      
         MVC   SUB1(4),=C'PSW='                                                 
         GOTO1 (RF),(R1),DMPPSWD,SUB1+4,4,=C'TOG'                               
         GOTO1 (RF),(R1),DMPPSWD+4,SUB1+13,4,=C'TOG'                            
         UNPK  DUB(6),DMPTIME                                                   
         OI    DUB+5,X'F0'                                                      
         CLI   DUMPTYPE,0          FORMAT TITLE                                 
         BE    *+10                                                             
         MVC   DTYP(4),=C'MINI'                                                 
         MVC   DNUM(1),REQDUMP                                                  
         OI    DNUM,X'F0'                                                       
         MVC   DHR(2),DUB                                                       
         MVC   DMIN(2),DUB+2                                                    
         MVC   DSEC(2),DUB+4                                                    
         MVC   TITLE,DTITLE                                                     
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         EJECT                                                                  
         MVC   PARSTART,DMPPART    SAVE PARTITION START ADDRESS                 
         MVI   PARSTART,0          CLEAR HOB                                    
         MVC   SAVEHEAD,DMPHDR     SAVE FACPAK HEADER                           
         MVI   DMPSWT,1            SPECIAL FUNCTION                             
         MVC   DMPDESC,=C'*FACILS*'                                             
         MVC   DMPDUMP,DMPFACS                                                  
         BAS   RE,DUMPPT           STORAGE MAP 1                                
*                                                                               
         MVI   DMPSWT,2            SPECIAL FUNCTION                             
         MVC   DMPDESC,=C'*TENTRY*'                                             
         MVC   DMPDUMP,DMPTCBE                                                  
         BAS   RE,DUMPPT           STORAGE MAP 2                                
*                                                                               
         MVI   DMPSWT,3            SPECIAL FUNCTION                             
         MVC   DMPDESC,=C'*PGMMAP*'                                             
         MVC   DMPDUMP,ATCBMAP                                                  
         BAS   RE,DUMPPT           STORAGE MAP 3                                
*                                                                               
         MVC   DMPHDR(24),SAVEHEAD RESTORE FACPAK HEADER                        
         MVI   DMPSWT,0            PRINT DUMP                                   
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
*                                                                               
         CLI   DUMPTYPE,0                                                       
         BNE   *+12                                                             
         BAS   RE,DUMPPT                                                        
         B     DMPX                                                             
*                                                                               
         MVC   DMPHDR(24),DMPCKPT                                               
         BAS   RE,DUMPPT                                                        
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         MVC   DMPHDR(24),DMPFIL1                                               
         BAS   RE,DUMPPT                                                        
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         MVC   DMPHDR(24),DMPPRGM                                               
         BAS   RE,DUMPPT                                                        
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         MVC   DMPHDR(24),DMPFIL2                                               
         BAS   RE,DUMPPT                                                        
*                                                                               
DMPX     B     INPUT                                                            
         EJECT                                                                  
DUMPPT   NTR1                      PRINT DUMP USING DUMP HEADER                 
         CLI   DMPSWT,0                                                         
         BNE   *+10                                                             
         MVC   SUB1+62(8),DMPDESC                                               
         MVI   DMPDUMP,0           REMOVE ABENDWHY BEFORE CALC                  
         OC    DMPDUMP,DMPDUMP     FUDGE FOR NO FILES                           
         BZ    DUMPXT                                                           
         L     R3,DMPDUMP                                                       
         LH    R2,DMPPAGE          WORK OUT END ADDRESS                         
         SLL   R2,11                                                            
         AR    R2,R3                                                            
         ST    R2,END                                                           
         CLI   DMPSWT,0            IF SPECIAL FUNCTION IGNORE ROUNDING          
         BNE   *+12                                                             
         SRL   R3,5                MOD START ADDRESS TO 32 BYTES                
         SLL   R3,5                                                             
         ST    R3,START                                                         
         S     R3,PARSTART                                                      
         SRL   R3,11                                                            
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)                                                   
         ST    R3,BLKNUM           SAVE RELATIVE BLOCK NUMBER                   
*                                                                               
         SR    R2,R2                                                            
         D     R2,RECTRK           R2=REC NUM ON TRK - 1                        
         STC   R2,ADDR+2                                                        
         LA    R3,1(R3)            ADJUST FOR HDR REC ON FIRST TRK              
         ICM   R2,3,ADDRSTR                                                     
         AR    R3,R2               R3=TRK NUM                                   
         STH   R3,ADDR                                                          
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,DMPPAGE                                                     
         SR    R2,R2                                                            
         D     R2,=A(BLKFCTR)                                                   
         L     R2,BLKNUM                                                        
         LA    R2,1(R2)                                                         
         SR    R3,R2                                                            
         ST    R3,BLOCKS           SAVE NUMBER OF BLOCKS TO PRINT               
*                                                                               
         MVI   DONESW,0            SET PRINT/READ INDICS                        
         MVI   SAMIND,0                                                         
*                                                                               
DUMPPT2  L     R4,START                                                         
         S     R4,PARSTART                                                      
         SRDL  R4,RSHIFT                                                        
         SRL   R5,32-RSHIFT                                                     
         LA    R5,DMPRC(R5)        R5=START POSITION IN BLOCK                   
         BAS   RE,READ                                                          
         BNZ   DMPERR                                                           
         CLI   DMPSWT,0            STORAGE MAP 1/2 (SPECIAL)                    
         BNE   DUMPPT20                                                         
*                                                                               
DUMPPT4  LA    R6,DMPRC                                                         
         A     R6,=A(RLEN)                                                      
         SR    R6,R5               R6=NUMBER OF BYTES TO PRINT                  
         CH    R6,=H'32'                                                        
         BH    DUMPPT6                                                          
*                                                                               
         LA    R7,DMPRC                                                         
         A     R7,=A(RLEN-36)                                                   
         MVC   DMPBACK,0(R7)       SAVE END OF BLOCK                            
         S     R5,=A(RLEN)         AND POSITION BLOCK POINTER                   
         XC    DMPRC(32),DMPRC                                                  
*                                                                               
         OC    BLOCKS,BLOCKS       LAST BLOCK ?                                 
         BNZ   *+12                                                             
         MVI   DONESW,1            SET LAST INDIC                               
         B     DUMPPT6                                                          
*                                                                               
         L     R0,BLOCKS           DECREMENT BLOCK COUNTER                      
         BCTR  R0,0                                                             
         ST    R0,BLOCKS                                                        
         BAS   RE,READ             READ DUMP BLOCK                              
         BNZ   DMPERR                                                           
*                                                                               
DUMPPT6  CLC   1(31,R5),0(R5)      LINE OF DUP CHRS                             
         BNE   DUMPPT10                                                         
         CLI   SAMIND,1            SET DUP INDIC                                
         MVI   SAMIND,1                                                         
         BNE   DUMPPT8                                                          
         BCTR  R5,0                                                             
         CLC   0(1,R5),1(R5)       SAME AS LAST LINE ?                          
         LA    R5,1(R5)                                                         
         BE    DUMPPT18            YES - IGNORE THIS LINE                       
*                                  NO - SPECIAL FORMAT                          
DUMPPT8  GOTO1 VHEXOUT,DMCB,(R5),P+09,4,=C'TOG'                                 
         MVC   P+18(8),=C'--SAME--'                                             
         B     DUMPPT16                                                         
*                                  STANDARD FORMAT                              
DUMPPT10 LR    R3,R5                                                            
         LA    R4,P+09                                                          
         LA    R7,4                                                             
         LA    R6,2                                                             
*                                                                               
DUMPPT12 GOTO1 VHEXOUT,DMCB,(R3),(R4),4,=C'TOG'                                 
         LA    R3,4(R3)                                                         
         LA    R4,09(R4)                                                        
         BCT   R7,DUMPPT12                                                      
         LA    R4,2(R4)                                                         
         LA    R7,4                                                             
         BCT   R6,DUMPPT12                                                      
         MVI   SAMIND,0                                                         
*                                  LINE ADDR/ALPHA TRANSLATION                  
DUMPPT16 GOTO1 VHEXOUT,DMCB,START,DUB,4                                         
         MVC   P(6),DUB+2                                                       
         MVC   P+86(16),0(R5)                                                   
         MVC   P+104(16),16(R5)                                                 
         GOTO1 VDUMPOUT,DMCB,(34,P+86),0,0                                      
         GOTO1 VPRINTER                                                         
*                                                                               
DUMPPT18 L     R4,START            BUMP LINE ADDR                               
         LA    R4,32(R4)                                                        
         ST    R4,START                                                         
         CLC   START,END                                                        
         BH    DUMPXT                                                           
         LA    R5,32(R5)           BUMP BLOCK POINTER                           
         CLI   DONESW,1            LAST BLOCK ?                                 
         BNE   DUMPPT4                                                          
         LA    R6,DMPRC                                                         
         CR    R5,R6                                                            
         BL    DUMPPT4                                                          
         B     DUMPXT              DONE                                         
*                                                                               
DUMPPT20 LA    R6,DMPRC            GET 256 BYTES INTO SAVEAREA                  
         A     R6,=A(RLEN)                                                      
         SR    R6,R5                                                            
         LA    R1,256                                                           
         CR    R6,R1               BOUNDARY CONDITION                           
         BL    *+14                                                             
         MVC   SAVEAREA,0(R5)                                                   
         B     DUMPPT22                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   SAVEAREA(0),0(R5)   SAVE BYTES FROM THIS BLOCK                   
         SR    R1,R6                                                            
         LA    R6,SAVEAREA(R6)                                                  
         BAS   RE,READ             AND GET NEXT FOR REMAINDER                   
         BNZ   DMPERR                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),DMPRC                                                    
*                                                                               
DUMPPT22 CLI   DMPSWT,1                                                         
         BNE   *+12                                                             
         BAS   RE,DUMPFAC          FACILITIES MAP                               
         B     DUMPXT                                                           
         CLI   DMPSWT,2                                                         
         BNE   *+12                                                             
         BAS   RE,DUMPTCB          TCB MAP                                      
         B     DUMPXT                                                           
         CLI   DMPSWT,3            PROGRAM AREA MAP                             
         BNE   *+12                                                             
         BAS   RE,DUMPMAP                                                       
         B     DUMPXT                                                           
*                                  A. N. OTHER                                  
DUMPXT   XIT1                                                                   
         EJECT                                                                  
DUMPFAC  NTR1                      FACILITIES STORAGE MAP                       
         LA    R7,SAVEAREA                                                      
         USING SYSFACD,R7                                                       
         MVC   SUB1+27(7),=C'PSW-RB='                                           
         L     R2,DMPPSWD+4                                                     
         LA    R2,0(R2)                                                         
         L     R3,DMPREGS+8                                                     
         LA    R3,0(R3)                                                         
         SR    R2,R3                                                            
         ST    R2,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                  
         MVC   SUB1+34(6),DUB+2                                                 
         CLI   DMPPART,C'I'        TEST PC OR IT DUMP                           
         BNE   *+10                                                             
         MVC   SUB1+42(10),=C'** LOOP **'                                       
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'PARTITION MAP'                                          
         BASR  RE,RF                                                            
         MVC   P(23),=23C'-'                                                    
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         LA    R6,FACTAB                                                        
*                                                                               
DUMPFAC2 CLI   0(R6),X'FF'                                                      
         BE    DUMPXT                                                           
         SR    R1,R1                                                            
         EX    R1,0(R6)            GET ADDRESS IN R0                            
         ST    R0,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                  
         MVC   P(16),4(R6)                                                      
         MVC   P+17(6),DUB+2                                                    
         BAS   RE,WHATREG                                                       
         GOTO1 VPRINTER                                                         
         LA    R6,L'FACTAB(R6)                                                  
         B     DUMPFAC2                                                         
         DROP  R7                                                               
         EJECT                                                                  
DUMPTCB  NTR1                      TCB ENTRY STORAGE MAP                        
         LA    R7,SAVEAREA                                                      
         USING TCBD,R7                                                          
         MVC   ATCBMAP,TCBMAP      SAVE FOR PROGRAM MAP                         
         GOTO1 VPRINTER                                                         
         BASR  RE,RF                                                            
         MVC   P(10),=C'TASK#N MAP'                                             
         MVC   P+5(1),TCBID+6                                                   
         BASR  RE,RF                                                            
         MVC   P(23),=23C'-'                                                    
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         MVC   P(4),=C'LUID'                                                    
         MVC   P+15(8),TCBSYM                                                   
         BASR  RE,RF                                                            
         LA    R6,TCBTAB                                                        
*                                                                               
DUMPTCB2 CLI   0(R6),X'FF'                                                      
         BE    DUMPXT                                                           
         SR    R1,R1                                                            
         EX    R1,0(R6)            GET ADDRESS IN R0                            
         ST    R0,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                  
         MVC   P(16),4(R6)                                                      
         MVC   P+17(6),DUB+2                                                    
         BAS   RE,WHATREG                                                       
         GOTO1 VPRINTER                                                         
         LA    R6,L'TCBTAB(R6)                                                  
         B     DUMPTCB2                                                         
         DROP  R7                                                               
         EJECT                                                                  
DUMPMAP  NTR1                      PROGRAM AREA STORAGE MAP                     
         LA    R7,SAVEAREA                                                      
         GOTO1 VPRINTER                                                         
         BASR  RE,RF                                                            
         MVC   P(16),=C'PROGRAM AREA MAP'                                       
         BASR  RE,RF                                                            
         MVC   P(23),=23C'-'                                                    
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         XC    DUB,DUB                                                          
         MVC   DUB(2),2(R7)                                                     
         GOTO1 VHEXOUT,DMCB,DUB,WORK2,3,=C'TOG'                                 
         MVI   WORK2,C'T'                                                       
         L     R0,8(R7)                                                         
         ST    R0,FULL                                                          
         GOTO1 (RF),(R1),FULL,DUB,4,=C'TOG'                                     
         MVC   P(6),WORK2                                                       
         MVC   P+17(6),DUB+2                                                    
         BAS   RE,WHATREG                                                       
         GOTO1 VPRINTER                                                         
         CLI   12(R1),0                                                         
         BE    DUMPMAP2                                                         
         L     R0,12(R7)                                                        
         LTR   R0,R0                                                            
         BZ    DUMPMAP2                                                         
         ST    R0,FULL                                                          
         GOTO1 VHEXOUT,DMCB,12(R7),WORK2+4,1,=C'TOG'                            
         GOTO1 (RF),(R1),FULL,DUB,4,=C'TOG'                                     
         MVC   P(6),WORK2                                                       
         MVC   P+17(6),DUB+2                                                    
         BAS   RE,WHATREG                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
DUMPMAP2 LA    RE,P                TEST LEVEL                                   
         CLI   1(R7),0                                                          
         BE    DUMPMAP4                                                         
         MVC   P(7),=C'TEST= ,'                                                 
         MVC   P+5(1),1(R7)                                                     
         OI    P+5,X'C0'                                                        
         LA    RE,6(RE)                                                         
*                                                                               
DUMPMAP4 TM    0(R7),X'40'         CIL MODE                                     
         BZ    DUMPMAP6                                                         
         CLI   0(RE),C','                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         MVC   0(7,RE),=C'CIL=YES'                                              
         LA    RE,7(RE)                                                         
*                                                                               
DUMPMAP6 MVI   0(RE),C' '                                                       
         CLI   P,C' '                                                           
         BE    DUMPXT                                                           
         GOTO1 VPRINTER                                                         
         B     DUMPXT                                                           
         EJECT                                                                  
WHATREG  NTR1                      FIND REGS POINTING TO ENTRYS                 
         LA    R2,P+25                                                          
         LA    R3,WORK                                                          
         LA    R4,16                                                            
         LA    R5,REGS                                                          
*                                                                               
WHATREG2 CLC   1(3,R3),FULL+1                                                   
         BNE   *+18                                                             
         MVC   0(2,R2),0(R5)                                                    
         MVI   2(R2),C','                                                       
         LA    R2,3(R2)                                                         
*                                                                               
         LA    R3,4(R3)                                                         
         LA    R5,2(R5)                                                         
         BCT   R4,WHATREG2                                                      
         BCTR  R2,0                                                             
         MVI   0(R2),C' '                                                       
*                                                                               
WHATREGX XIT1                                                                   
         EJECT                                                                  
READ     NTR1                      READ NEXT DUMP RECORD                        
         LA    RE,DMPRC                                                         
         L     RF,BLKSIZE                                                       
         XCEF                                                                   
*                                                                               
RD1      SR    RE,RE               GET LAST BLOCK NUMBER                        
         IC    RE,ADDR+2                                                        
         C     RE,RECTRK                                                        
         BE    RD2                                                              
         LA    RE,1(RE)            BUMP BLOCK NUMBER                            
         STC   RE,ADDR+2                                                        
         B     RD3                                                              
RD2      ICM   RE,3,ADDR                                                        
         LA    RE,1(RE)            BUMP TRACK NUMBER                            
         STH   RE,ADDR                                                          
         MVI   ADDR+2,1                                                         
         CLC   ADDR(2),ADDREND                                                  
         BNH   RD3                                                              
         B     RDX  <==== DON'T DIE - JUST RETURN CC NE                         
         DC    H'0'                DIE IF OUT OF RANGE                          
*                                                                               
RD3      GOTO1 DADDS,P1,V(RDID)                                                 
         OC    P3(2),P3                                                         
RDX      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DADDS    DC    V(DADDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VCARDS   DC    V(CARDS)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VDUMPOUT DC    V(DUMPOUT)                                                       
*                                                                               
DMPFILE  DMDA  DSKXTNT=16                                                       
*                                                                               
BLDXTNT  DS    0F                                                               
*                                                                               
BLKSIZE  DC    F'8192'                                                          
         EJECT                                                                  
         USING SYSFACD,R7          STORAGE MAP TABLES                           
FACTAB   DS    0CL20                                                            
         L     R0,DMPDUMP                                                       
         DC    CL16'SYSFACS'                                                    
         L     R0,VDATAMGR                                                      
         DC    CL16'DATAMGR'                                                    
         L     R0,VDMOD000                                                      
         DC    CL16'DMCNTL'                                                     
         L     R0,VDADDS                                                        
         DC    CL16'DMDA'                                                       
         L     R0,VPHLIST                                                       
         DC    CL16'PHASE LIST'                                                 
         L     R0,VADRBUFF                                                      
         DC    CL16'ADR BUFFER'                                                 
         L     R0,VTCB                                                          
         DC    CL16'TCB LIST'                                                   
         L     R0,VSSB                                                          
         DC    CL16'SSB'                                                        
         L     R0,VSELIST                                                       
         DC    CL16'SELIST'                                                     
         L     R0,VTSTTAB                                                       
         DC    CL16'TSTTAB'                                                     
         L     R0,VLOCKTAB                                                      
         DC    CL16'LOCKTAB'                                                    
         L     R0,VCHKPT1                                                       
         DC    CL16'CHKPT1 START'                                               
         L     R0,VCHKPT1X                                                      
         DC    CL16'CHKPT1 END'                                                 
         L     R0,VCHKPT2                                                       
         DC    CL16'CHKPT2 START'                                               
         L     R0,VCHKPT2X                                                      
         DC    CL16'CHKPT2 END'                                                 
         L     R0,VUTL                                                          
         DC    CL16'UTL LIST'                                                   
         L     R0,VUPDTAB                                                       
         DC    CL16'UPDTAB'                                                     
         DC    X'FFFF'                                                          
         EJECT                                                                  
         USING TCBD,R7                                                          
TCBTAB   DS    0CL20                                                            
         L     R0,DMPDUMP                                                       
         DC    CL16'TCB ENTRY'                                                  
         L     R0,TCBPAR5                                                       
         DC    CL16'SELIST ENTRY'                                               
         L     R0,TCBPAR3                                                       
         DC    CL16'UTL ENTRY'                                                  
         L     R0,TCBDTFS                                                       
         DC    CL16'TASK DTF START'                                             
         L     R0,TCBDTFX                                                       
         DC    CL16'TASK DTF END'                                               
         L     R0,TCBFILES                                                      
         DC    CL16'TASK BUFF START'                                            
         L     R0,TCBFILEX                                                      
         DC    CL16'TASK BUFF END'                                              
         L     R0,TCBWRKA                                                       
         DC    CL16'TASK WORK AREA'                                             
         L     R0,TCBPAR2                                                       
         DC    CL16'TIA ADDRESS'                                                
         L     R0,TCBPAR6                                                       
         DC    CL16'TWA ADDRESS'                                                
         L     R0,TCBMAP                                                        
         DC    CL16'MAP ADDRESS'                                                
         L     R0,TCBPGMA                                                       
         DC    CL16'TASK PGM START'                                             
         L     R0,TCBPGMX                                                       
         DC    CL16'TASK PGM END'                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
MSGAREA  DS    CL10                                                             
*                                                                               
DTITLE   DS    0CL60                                                            
         DC    C'        FACPAK '                                               
DTYP     DC    C'MAXI DUMP NUMBER '                                             
DNUM     DC    C'N TAKEN AT '                                                   
DHR      DC    C'HH.'                                                           
DMIN     DC    C'MM.'                                                           
DSEC     DC    C'SS          '                                                  
*                                                                               
REGS     DC    C'R0R1R2R3R4R5R6R7R8R9RARBRCRDRERF'                              
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
DMCB     DS    6F                                                               
WORK     DS    16F                                                              
WORK2    DS    CL8                                                              
*                                                                               
RECTRK   DS    F                                                                
TRKCYL   DS    F                                                                
TOTTRKS  DS    F                                                                
TOTCYLS  DS    F                                                                
NUMTRKS  DS    F                                                                
NUMCYLS  DS    F                                                                
ACMRG    DS    F                                                                
ATCBMAP  DS    F                                                                
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
UPSI     DC    X'00'                                                            
MAXDUMP  DS    C                                                                
REQDUMP  DS    C                                                                
DUMPTYPE DS    C                                                                
*                                                                               
START    DS    F                                                                
END      DS    F                                                                
PARSTART DS    F                                                                
BLOCKS   DS    F                                                                
BLKNUM   DS    F                                                                
ADDR     DS    F                                                                
ADDRSTR  DS    F                                                                
ADDREND  DS    F                                                                
DMPSWT   DS    C                                                                
DONESW   DS    C                                                                
SAMIND   DS    C                                                                
         DS    C                                                                
*                                                                               
SAVEHEAD DS    CL24                                                             
         DS    0D                  ALIGN                                        
SAVEAREA DS    CL256                                                            
*                                                                               
BLKFCTR  EQU   4                                                                
RSHIFT   EQU   13                                                               
RLEN     EQU   BLKFCTR*2048                                                     
*                                                                               
DMPHD    DS    (DMPHDRL)C                                                       
*                                                                               
DMPBACK  DS    CL36                                                             
DMPRC    DS    8192C                                                            
*                                                                               
DUMPWORK DS    512D                                                             
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013FADUMPS   05/01/02'                                      
         END                                                                    
