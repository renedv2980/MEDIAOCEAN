*          DATA SET DMDDPRT    AT LEVEL 003 AS OF 05/07/08                      
*PHASE DMDDPRTA                                                                 
*INCLUDE DMDADDS                                                                
*INCLUDE DMDYNDD                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
         TITLE 'DMDDPRT - READ IS OR DA FILE BLOCK AND PRINT'                   
*                                                                               
* Program will read blocks from any DDS file and print each block in            
* dump format. The program does not deblock, so will print only full            
* blocks. Any type of DDS file is supported.                                    
*                                                                               
* The blocks to be printed can be specified as one or more DDS format           
* disk addresses or all records in a track can be printed.                      
*                                                                               
* Parameter cards must start in column 1. No abbreviated forms allowed          
* Each card is read and processed separately and different types can be         
* mixed. There is no limit to the number of parameter cards.                    
*                                                                               
* A DD card for DDname=DDFILE is required, similar to that shown below:         
*                                                                               
* //DDFILE   DD DSN=ACC.ACCDIR0,DISP=SHR                                        
*                                                                               
* To print individual blocks                                                    
* --------------------------                                                    
*                                                                               
* Disk addresses must be specified in unblocked format. I.e. the record         
* number must be specified as zero.                                             
*                                                                               
* To dump a block using a 20 bit disk address                                   
*                                                                               
* DUMP=TTTTT0BB                                                                 
* DUMP20=TTTTT0BB                                                               
*                                                                               
* To dump a block using an 18 bit disk address                                  
*                                                                               
* DUMP18=TTTTXB00 (X=TB)                                                        
*                                                                               
* To dump a block using a 16 bit disk address                                   
*                                                                               
* DUMP16=TTTTBB00                                                               
*                                                                               
* To print a whole track                                                        
* ----------------------                                                        
*                                                                               
* TRACK=ttttttt   Decimal track number, up to 7 digits. Max=1048575             
*                                                                               
* TRACK=Xttttt    Hex track number, up to 5 digits. Max=XFFFFF                  
         EJECT                                                                  
DMDDPRT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**DDPRT*,=A(REGSAVE),CLEAR=YES                       
         USING WORKD,RC                                                         
*                                                                               
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
*                                                                               
         L     RF,=A(IOAREA-WORKD)                                              
         AR    RF,RC                                                            
         ST    RF,AIO                                                           
*                                                                               
         USING DTFPHD,R9                                                        
         LA    R9,DDFILE                                                        
         OI    DTFOPEN,X'80'       MAKE IT READ-ONLY                            
*                                                                               
         L     R2,AIO                                                           
         L     R3,DTFADCB                                                       
         LA    RF,DSNXTRCT                                                      
         STCM  R2,7,1(RF)          SET OUTPUT ADDRESS IN EXLST                  
         STCM  RF,7,X'25'(R3)      SET EXLST ADDRESS IN DCB                     
         RDJFCB ((3))                                                           
         MVC   DSN,0(R2)           EXTRACT DSN FOR TITLE                        
*                                                                               
         GOTO1 VDADDS,P1,14,,,(R9)       OPEN THE FILE                          
*                                                                               
         MVC   TITLE,PRTTITLE                                                   
*                                                                               
GETCARD  MVI   TRKMODE,C'N'                                                     
         GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD         END OF CARDS?                                
         BE    CLOSE               YES                                          
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
         CLI   CARD,C'*'           IGNORE COMMENTS                              
         BE    GETCARD                                                          
*                                                                               
GCD10    CLC   =C'DUMP16=',CARD    16 BIT ADDRESS: TTTTBB00                     
         BNE   GCD20               CONVERT TO 20 BIT                            
         GOTO1 VHEXIN,DMCB,CARD+7,DA,8                                          
         OC    DMCB+12(4),DMCB+12                                               
         BZ    GCD90                                                            
         CLC   DMCB+12(4),=F'4'                                                 
         BNE   GCD90                                                            
         CLI   DA+3,0                                                           
         BNE   GCD90                                                            
         XR    RE,RE               00000000/........                            
         L     RF,DA               00000000/TTTTBB00                            
         SLDL  RE,16               0000TTTT/BB000000                            
         SRL   RF,4                0000TTTT/0BB00000                            
         SRDL  RE,20               00000000/0TTTT0BB                            
         ST    RF,DA                                                            
         BAS   RE,PRTBLK                                                        
         B     GETCARD                                                          
*                                                                               
GCD20    CLC   =C'DUMP18=',CARD    18 BIT ADDRESS: TTTTXB00 X=T0                
         BNE   GCD30               CONVERT TO 20 BIT (BLK <= 63)                
         GOTO1 VHEXIN,DMCB,CARD+7,DA,8                                          
         OC    DMCB+12(4),DMCB+12                                               
         BZ    GCD90                                                            
         CLC   DMCB+12(4),=F'4'                                                 
         BNE   GCD90                                                            
         CLI   DA+3,0                                                           
         BNE   GCD90                                                            
         XR    RE,RE               00000000/........                            
         L     RF,DA               00000000/TTTTXB00 X=TB                       
         SLDL  RE,18               000XTTTT/BY000000 X=0T,Y=B0                  
         SRL   RF,6                000XTTTT/0BB00000 X=0T                       
         SRDL  RE,20               00000000/XTTTT0BB X=0T                       
         ST    RF,DA                                                            
         BAS   RE,PRTBLK                                                        
         B     GETCARD                                                          
*                                                                               
GCD30    LA    RE,CARD+5           20 BIT ADDRESS: TTTTT0BB                     
         CLC   =C'DUMP=',CARD      NO CONVERSION NEEDED                         
         BE    GCD32                                                            
         LA    RE,CARD+7                                                        
         CLC   =C'DUMP20=',CARD                                                 
         BNE   GCD40                                                            
GCD32    GOTO1 VHEXIN,DMCB,(RE),DA,8                                            
         OC    DMCB+12(4),DMCB+12                                               
         BZ    GCD90                                                            
         CLC   DMCB+12(4),=F'4'                                                 
         BNE   GCD90                                                            
         TM    DA+2,X'0F'                                                       
         BNZ   GCD90                                                            
         BAS   RE,PRTBLK                                                        
         B     GETCARD                                                          
*                                                                               
GCD40    CLC   =C'TRACK=',CARD     DUMP WHOLE TRACK                             
         BNE   GCD50                                                            
         MVI   TRKMODE,C'Y'                                                     
         CLI   CARD+6,C'X'         TRACK NUM IN HEX?                            
         BE    GCD45                                                            
*                                                                               
         LA    RE,CARD+6           VALIDATE DECIMAL TRACK NUMBER                
         LR    RF,RE                                                            
         LA    R0,8                MAX 7 DIGITS PLUS BLANK                      
GCD41    CLI   0(RE),C' '                                                       
         BE    GCD42                                                            
         CLI   0(RE),C'0'                                                       
         BL    GCD90               NOT NUMERIC                                  
         LA    RE,1(,RE)                                                        
         BCT   R0,GCD41                                                         
         B     GCD90               TOO LONG                                     
GCD42    SR    RE,RF               RE=LENGTH                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CARD+6(0)                                                    
         CVB   RF,DUB              000TTTTT                                     
         CL    RF,=X'000FFFFF'                                                  
         BH    GCD90               TOO BIG                                      
         SLL   RF,12               TTTTT000                                     
         ST    RF,DA                                                            
         MVI   TRKMODE,C'Y'                                                     
         BAS   RE,PRTBLK                                                        
         B     GETCARD                                                          
*                                                                               
GCD45    LA    RE,CARD+7           VALIDATE HEX TRACK NUMBER                    
         LR    RF,RE                                                            
         LA    R0,6                MAX 5 DIGITS PLUS BLANK                      
GCD46    CLI   0(RE),C' '                                                       
         BE    GCD47                                                            
         LA    RE,1(,RE)                                                        
         BCT   R0,GCD46                                                         
         B     GCD90               TOO LONG                                     
GCD47    SR    RE,RF               RE=LENGTH                                    
         MVC   WORK(5),=C'00000'                                                
         LA    RF,WORK+6                                                        
         SR    RF,RE                                                            
         MVC   0(6,RF),CARD+7                                                   
         GOTO1 VHEXIN,DMCB,WORK,DA,6                                            
         OC    DMCB+12(4),DMCB+12                                               
         BZ    GCD90                                                            
         CLC   DMCB+12(4),=F'3'                                                 
         BNE   GCD90                                                            
         CLC   DA(3),=X'0FFFFF'                                                 
         BH    GCD90                                                            
         MVI   DA+3,0                                                           
         L     RF,DA               0TTTTT00                                     
         SLL   RF,4                TTTTT000                                     
         ST    RF,DA                                                            
         MVI   TRKMODE,C'Y'                                                     
         BAS   RE,PRTBLK                                                        
         B     GETCARD                                                          
*                                                                               
GCD50    EQU   GCD90                                                            
*                                                                               
GCD90    MVC   P(10),=C'***INVALID'                                             
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     GETCARD                                                          
         EJECT                                                                  
*        END OF CARDS                                                           
*                                                                               
CLOSE    GOTO1 VDADDS,P1,15,,,(R9)       CLOSE THE FILE                         
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
*        READ AND PRINT THE BLOCK WHOSE ADDRESS IS IN DA                        
*                                                                               
PRTBLK   NTR1                                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
PRTB10   CLI   TRKMODE,C'Y'        IF TRACK MODE, INCREMENT DA                  
         BNE   PRTB12                                                           
         L     RF,DA                                                            
         AHI   RF,1                                                             
         ST    RF,DA                                                            
*                                                                               
PRTB12   MVC   P(5),=C'TRACK'                                                   
         L     R1,DA                                                            
         SRL   R1,12                                                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+6(7),DUB                                                       
*                                                                               
         MVC   P+13(7),=C', Block'                                              
         ICM   R1,B'1000',DA+3                                                  
         SRL   R1,24                                                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+21(3),DUB                                                      
*                                                                               
         BAS   RE,GETBLK           READ THE BLOCK                               
         BE    PRTB20              FOUND OK                                     
         GOTO1 VPRINTER            GETBLK STICKS MESSAGE IN P                   
         B     PRTBX                                                            
*                                                                               
*        PRINT THE BLOCK                                                        
*                                                                               
PRTB20   LH    R3,P3+2             LENGTH OF RECORD                             
*                                                                               
         MVC   P+24(11),=C', Length X'''                                        
         STH   R3,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,P+35,2                                         
         MVI   P+39,C''''                                                       
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+41(5),DUB                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   MID1(3),=C'Hex'                                                  
         MVC   MID1+5(3),=C'Dec'                                                
         MVC   MID1+12(32),=C'0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.'                 
         MVC   MID1+45(32),=C'0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.'                 
         MVC   MID1+79(16),=C'0123456789ABCDEF'                                 
         MVC   MID1+95(16),=C'0123456789ABCDEF'                                 
         MVC   P,MID1                                                           
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R4,AIO                                                           
         XR    R5,R5                                                            
         XR    R6,R6                                                            
         MVI   SAME,C'N'                                                        
*                                                                               
PRTB30   LA    R2,32               BYTES TO DUMP PER LINE                       
*                                                                               
         CR    R2,R3               R3=LENGTH LEFT                               
         BL    *+8                                                              
         LR    R2,R3               IF RECORD <= ONE LINE, SHORT PRINT           
         XR    R6,R6               AND NO '--SAME--'                            
*                                                                               
         MVC   WORK,SPACES         EDIT OUT DISPLACEMENT IN HEX AND DEC         
         STH   R5,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,WORK,2                                         
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+5(5),DUB                                                    
*                                                                               
         LTR   R6,R6               HAVE WE GOT A PREVIOUS LINE                  
         BZ    PRTB40              NO                                           
         CLC   0(32,R6),0(R4)      IS IT THE SAME                               
         BNE   PRTB40              NO                                           
         CLI   SAME,C'Y'           DID WE ALREADY SAY SO                        
         BE    PRTB90              YES, DON'T PRINT THIS LINE                   
         MVC   P(10),WORK          MOVE IN DISPLACEMENT                         
         MVC   P+12(8),=C'--Same--'                                             
         MVI   SAME,C'Y'                                                        
         GOTO1 VPRINTER                                                         
         B     PRTB90                                                           
*                                                                               
PRTB40   LR    R6,R4               SAVE THIS LINE START                         
         MVI   SAME,C'N'           NOT SAME AS LAST                             
         MVC   P(10),WORK          MOVE IN DISPLACEMENT                         
*                                                                               
         MVC   WORK,SPACES         EDIT LINE OUT IN HEX                         
         GOTO1 VHEXOUT,DMCB,(R4),WORK,(R2)                                      
         MVC   P+12(32),WORK                                                    
         MVC   P+45(32),WORK+32                                                 
*                                                                               
         MVC   WORK,SPACES         AND AGAIN IN EBCDIC                          
         LR    RE,R2                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P+79(0),0(R4)                                                    
*                                                                               
         LA    RF,P+79             NOW REPLACE LOW CHARS WITH DOTS              
         LA    R1,32                                                            
         BASR  RE,0                                                             
         CLI   0(RF),C' '                                                       
         BNL   *+8                                                              
         MVI   0(RF),C'.'                                                       
         LA    RF,1(,RF)                                                        
         BCTR  R1,RE                                                            
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
PRTB90   SR    R3,R2               DECREMENT LENGTH LEFT                        
         BZ    PRTBX               ALL DONE                                     
         AR    R4,R2               INCREMENT LOCATION IN RECORD                 
         AR    R5,R2               INCREMENT DISPLACEMENT                       
         B     PRTB30                                                           
*                                                                               
PRTBX    ZAP   LINE,=P'99'                                                      
         MVC   MID1,SPACES                                                      
         CLI   TRKMODE,C'Y'        IF TRACK MODE, PRINT NEXT BLOCK              
         BE    PRTB10                                                           
         XIT1                                                                   
         EJECT                                                                  
* READ BLOCK WHOSE ADDRESS IS IN DA                                             
*                                                                               
GETBLK   LR    R0,RE                                                            
         MVC   P6(4),DA            SET TTTTBB00                                 
         GOTO1 VDADDS,P1,01,AIO,0,,P6                                           
         OC    P3(2),P3                                                         
         BNZ   GETBLK1             CC NEQ/NOT 0 = BLOCK NOT FOUND               
         OC    P3+2(2),P3+2        TEST LENGTH                                  
         BNZ   GETBLKY             IF NOT ZERO, BLOCK IS GOOD                   
         MVI   P+24,C','                                                        
         MVC   P+26(23),=C'*** Zero Length Rec ***'                             
         B     GETBLKU             CC LO = ZERO LENGTH RECORD                   
GETBLKY  CR    RB,RB               SET CC EQ                                    
         B     GETBLKX             CC EQ = OK                                   
GETBLK1  MVI   P+24,C','                                                        
         MVC   P+26(23),=C'*** End of Track ***   '                             
         TM    P3+1,X'08'                                                       
         BO    GETBLKE             CC HI = BLOCK NOT FOUND                      
         MVC   P+26(23),=C'*** End of File ***    '                             
         TM    P3+1,X'04'                                                       
         BO    GETBLKE             CC HI = EOF                                  
         MVC   P+26(23),=C'*** Unrec IO Error *** '                             
         CLI   P3,X'80'                                                         
         BO    GETBLKU             CC LO = OTHER ERROR                          
         MVC   P+26(23),=C'*** Unknown Error ***  '                             
         B     GETBLKU             CC LO = UNKNOWN ERROR                        
GETBLKU  MVC   P+50(3),=C'P3='                                                  
         GOTO1 VHEXOUT,DMCB,P3,P+53,4                                           
         CR    RB,R0               RB MUST BE LOW                               
         B     GETBLKT                                                          
GETBLKE  CR    R0,RB               R0 MUST BE HIGH                              
GETBLKT  MVI   TRKMODE,C'N'        STOP TRACK MODE ON ANY ERROR                 
GETBLKX  LR    RE,R0                                                            
         BR    RE                                                               
         SPACE                                                                  
         ENTRY ADWAIT                                                           
         USING *,RF                                                             
ADWAIT   DS    0H                                                               
         LTR   RE,RE               RE IS NEG IF 31 BIT CALLER                   
         BP    ADWAITX                                                          
         AP    IOCOUNT,PCKD1                                                    
ADWAITX  BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
************************************************************                    
*                                                          *                    
************************************************************                    
         SPACE 1                                                                
IOCOUNT  DC    PL4'0'                                                           
PCKD1    DC    PL1'1'                                                           
         DS    0D                                                               
VCPRINT  DC    V(CPRINT)                                                        
VDADDS   DC    V(DADDS)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VCARDS   DC    V(CARDS)                                                         
*                                                                               
PRTTITLE DC    CL(L'TITLE)' '                                                   
         ORG   PRTTITLE                                                         
         DC    C'IS/DA Block Print. File '                                      
DSN      DS    CL44                                                             
         ORG   ,                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
         EJECT                                                                  
         DS    0A                                                               
DSNXTRCT DC    X'87',AL3(0)                                                     
*                                                                               
         DS    0D                                                               
         PRINT GEN                                                              
DDFILE   DMDA  BLKSIZE=32760,DSKXTNT=16,BIG=BIG                                 
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
************************************************************                    
* SSB AND UTL CSECTS                                       *                    
************************************************************                    
         SPACE 1                                                                
SSB      CSECT                                                                  
         DC    X'0000',X'FF',X'88'                                              
         DC    X'00000000'                                                      
         DC    CL8' '                                                           
         DC    A(0),A(0)                                                        
         DC    XL32'00'                                                         
*                                                                               
UTL      CSECT                                                                  
         DC    10F'0'                                                           
*                                                                               
REGSAVE  DS    60000C                                                           
         EJECT                                                                  
************************************************************                    
* WORKING STORAGE                                          *                    
************************************************************                    
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
SAME     DS    C                                                                
TRKMODE  DS    C                                                                
         DS    CL3                                                              
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
WORK     DS    CL128                                                            
*                                                                               
DA       DS    F                                                                
*                                                                               
AIO      DS    A                                                                
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
IOAREA   DS    32760C                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
************************************************************                    
* OTHER DSECTS                                             *                    
************************************************************                    
         SPACE 1                                                                
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMDDPRT   05/07/08'                                      
         END                                                                    
