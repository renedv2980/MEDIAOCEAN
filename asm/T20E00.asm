*          DATA SET T20E00     AT LEVEL 039 AS OF 05/01/02                      
*PHASE T20E00C,+0,NOAUTO                                                        
         TITLE 'T20E00 - DEMOGAPHIC INFORMATION SYSTEM BASE'                    
T20E00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 825,T20E00                                                       
         USING GENOLD,RC                                                        
         USING T20EFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         BAS   RE,INITL                                                         
         MVC   VCOMFACS,16(R1)     SET COMFACS ADDRESS (IN TWA)                 
         LA    RE,WORKEND                                                       
         A     RE,=F'2700'                                                      
         ST    RE,ADEMTAB                                                       
* EDIT INPUT FIELDS                                                             
         LA    R2,DEMMSGH                                                       
         XC    DEMMSG,DEMMSG                                                    
         MVC   DEMMSG(23),=C'DEMO INFORMATION SYSTEM'                           
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,DEMACTH                                                       
* EDIT ACTION                                                                   
         LA    R5,ACTTAB                                                        
EDTACT   CLC   4(3,R5),FLDDATA                                                  
         BE    ACTOK                                                            
         LA    R5,7(R5)                                                         
         CLI   2(R5),0                                                          
         BNE   EDTACT                                                           
         LA    R3,INVACT                                                        
         B     ERROR                                                            
ACTOK    CLI   0(R5),0                                                          
         BNE   *+12                                                             
         LA    R3,INVFMT                                                        
         B     ERROR                                                            
         CLC   PREVACT,FLDDATA                                                  
         BE    *+8                                                              
         BAS   R3,CLRPREV                                                       
         MVC   PREVACT,FLDDATA                                                  
         MVC   SCON,0(R5)                                                       
         OI    FLDIIND,X'20'        VALIDATED                                   
         OI    FLDOIND,X'81'       FORCE MODIFY                                 
         CLC   PREVSCR,SCRENNO     HAVE CORRECT SCREEN                          
         BE    EDTSRC               YES - EDIT SQURCE                           
         XC    SCRNPAGE(2),SCRNPAGE                                             
         MVC   PREVSCR,SCRENNO      NO - GET CORRECT SCREEN                     
         MVC   SOLY,=X'D9020EFE'   BUILD OVERLAY PARAMETER                      
         MVC   SCROLY,SCRENNO      SET SCREEN NUMBER                            
         L     R2,SOLY                                                          
         GOTO1 VCALLOV,DMCB,DEMLAST,(R2)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SCRENNO,X'FE'                                                    
         BNE   EDTSRC                                                           
         XC    DEMMSG,DEMMSG                                                    
         MVC   DEMMSG(26),=C'ENTER DEMOS,DAYS,AND TIMES'                        
         EJECT                                                                  
* CLEAR PROTECTED FIELDS - THEN EDIT SQURCE                                     
*                                                                               
EDTSRC   LA    R2,DEMLAST                                                       
         LR    RE,R2                                                            
CHKIN    CLI   FLDILEN,0                                                        
         BE    *+6                                                              
         LR    RE,R2                                                            
         ZIC   R3,FLDLEN                                                        
         LA    R2,0(R3,R2)                                                      
         CLI   0(R2),0                                                          
         BNE   CHKIN                                                            
         LA    R2,DEMLAST                                                       
         SR    R3,R3                                                            
         CLI   FLDLEN,72                                                        
         BL    NEXTFLD+4                                                        
CLRPROT  SR    R3,R3                                                            
         IC    R3,FLDLEN                                                        
         SH    R3,=H'9'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         OC    FLDDATA(0),FLDDATA     * EXECUTED *                              
         BZ    NEXTFLD                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA     * EXECUTED *                              
         FOUT  (R2)                                                             
NEXTFLD  LA    R2,9(R3,R2)                                                      
         CLI   FLDLEN,0            END OF SCREEN                                
         BE    EDTSRC1              YES - EDIT SOURCE                           
         CLI   FLDLEN,50                                                        
         BNL   CLRPROT                                                          
         CR    R2,RE                                                            
         BH    CLRPROT                                                          
         IC    R3,FLDLEN                                                        
         LA    R2,0(R3,R2)                                                      
         B     NEXTFLD+4                                                        
*                                                                               
EDTSRC1  LA    R2,DEMSRCH          EDIT SOURCE                                  
         LA    R3,INVRSRC                                                       
         MVI   CMEDIA,C'T'                                                      
         CLI   FLDDATA,C'A'                                                     
         BE    SRCOK                                                            
         CLI   FLDDATA,C'N'                                                     
         BE    SRCOK                                                            
         MVI   CMEDIA,C'C'                                                      
         CLI   FLDDATA,C'B'                                                     
         BE    SRCOK                                                            
         CLI   FLDDATA,C'C'                                                     
         BE    SRCOK                                                            
         B     ERROR                                                            
SRCOK    MVI   CSOURCE,X'01'                                                    
         CLI   FLDDATA,C'N'                                                     
         BNE   *+8                                                              
         MVI   CSOURCE,X'02'                                                    
         CLI   FLDDATA,C'B'        BBM ON ARB FILE                              
         BNE   *+8                                                              
         MVI   CSOURCE,1                                                        
         CLI   FLDDATA,C'C'        SCI ON NSI FILE                              
         BNE   *+8                                                              
         MVI   CSOURCE,2                                                        
         CLC   CSOURCE,PREVSRC                                                  
         BE    SRCOK1                                                           
         MVC   PREVSRC,CSOURCE                                                  
         BAS   R3,CLRPREV                                                       
         LA    R3,INVRSRC                                                       
SRCOK1   MVI   BYTE,X'60'                                                       
         CLI   CMEDIA,C'C'                                                      
         BNE   *+8                                                              
         MVI   BYTE,X'61'                                                       
         LA    R9,WORKEND                                                       
         A     R9,=F'2700'                                                      
         GOTO1 VCALLOV,DMCB,(BYTE,(R9)),(RA)                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGYALPHA                                                
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         CLI   AGYPROF,C'2'                                                     
         BE    CHKFLDS                                                          
         CLI   CSOURCE,1                                                        
         BNE   *+16                                                             
         CLI   AGYPROF,C'1'                                                     
         BNE   ERROR                                                            
         B     CHKFLDS                                                          
         CLI   AGYPROF,C'0'                                                     
         BNE   ERROR                                                            
         EJECT                                                                  
* EDIT REQUIRED FIELDS - SEND SPACES TO FIELDS NOT REQUIRED                     
*                                                                               
CHKFLDS  LA    R2,DEMMARH                                                       
         TM    FLDREQ,X'20'                                                     
         BO    *+12                                                             
         BAS   R9,CLRFLD                                                        
         B     *+8                                                              
         BAS   R9,EDTMAR                                                        
         LA    R2,DEMSTAH                                                       
         TM    FLDREQ,1                                                         
         BO    *+12                                                             
         BAS   R9,CLRFLD                                                        
         B     *+8                                                              
         BAS   R9,EDTSTA                                                        
         LA    R2,DEMBOOKH                                                      
         TM    FLDREQ,2                                                         
         BO    *+12                                                             
         BAS   R9,CLRFLD                                                        
         B     *+8                                                              
         BAS   R9,EDTBOOK                                                       
         LA    R2,DEMHUTH                                                       
         TM    FLDREQ,4                                                         
         BO    *+12                                                             
         BAS   R9,CLRFLD                                                        
         B     *+8                                                              
         BAS   R9,EDTHUT                                                        
         LA    R2,DEMPAGH                                                       
         BAS   R9,EDTPAGE                                                       
         LA    R2,DMEDEM1H                                                      
         TM    FLDREQ,X'08'                                                     
         BZ    *+8                                                              
EDTDEM   BAS   R9,EDTDEM1                                                       
         LA    R2,DMEDA1H                                                       
         TM    FLDREQ,X'10'                                                     
         BZ    EDTSDON                                                          
EDTDYTM1 LA    R3,INPMIS                                                        
         TM    FNO,1               EXPLODE                                      
         BO    *+12                 NO                                          
         CLI   PREVPAG,0            YES - SECOND PAGE                           
         BNE   EDTSDON                YES                                       
         CLI   FLDILEN,0           ANY INPUT                                    
         BE    ERROR                                                            
*                                                                               
* EDIT AND CONVERT DAY AND TIME                                                 
*                                                                               
         MVI   BYTE,X'10'                                                       
         BAS   R9,GETOV                                                         
         CLI   BYTE2,0             ANY ERRORS                                   
         BE    EDTSDON              NO - EDITS ARE DONE                         
         B     EXXMOD               YES - SEND ERROR                            
         EJECT                                                                  
* EDITS ARE DONE - CALL OVERLAYS                                                
*                                                                               
EDTSDON  CLI   FNO,0                                                            
         BE    EXIT                                                             
         SR    R0,R0                                                            
         MVC   BYTE,OVLYNO                                                      
         BAS   R9,GETOV                                                         
*                                                                               
         CLI   SCRENNO,X'FE'                                                    
         BNE   EXXMOD                                                           
         LA    R2,DMEDA1H                                                       
         LA    R7,DYTMLST          SET DAY TIME POINTER                         
DCOMP1   MVC   DUB(6),0(R7)                                                     
         CLI   DUB,0                                                            
         BE    EXXMOD                                                           
         MVI   BYTE,X'11'                                                       
         BAS   R9,GETOV            BUILD INTERFACE                              
         SPACE 1                                                                
* GET DEMO LOOKUP MODULE ADDRESS *                                              
         SPACE 1                                                                
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A20'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CMEDIA,C'C'                                                      
         BNE   *+8                                                              
         OI    HALF,X'80'          SET CANADIAN LOOKUP IND                      
* TEST FOR NUMERIC STATION (REALLY MARKET)                                      
         TM    STAT,X'F0'                                                       
         BNO   *+10                                                             
         XC    BUYREC+6(3),BUYREC+6 CLEAR STATION CALL LETTERS                  
*                                                                               
         MVC   BUYREC+4(2),AGYMKT                                               
         LA    R9,BUYREC+108                                                    
         CLI   SPILLSW,2                                                        
         BNE   DCOMPOK1                                                         
         ZIC   R0,1(R9)            SET TO SPILL ELEMENT                         
         AR    R9,R0                                                            
*                                                                               
DCOMPOK1 MVC   DMWORK+8(4),VCOMFACS                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,BUYREC,(SPILLSW,(R9)),(HALF,HALF+1),          X        
               WORKEND,DMWORK                                                   
*                                                                               
         MVI   BYTE,X'12'                                                       
         BAS   R9,GETOV                                                         
         LA    R5,3                                                             
NEXTF    SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,NEXTF                                                         
         LA    R7,6(R7)                                                         
         CLI   0(R7),0                                                          
         BNE   DCOMP1                                                           
         LA    R2,DMEDA1H          SET CURSOR TO DAY & TIME                     
         OI    FLDOIND,X'40'                                                    
         B     EXXMOD                                                           
         EJECT                                                                  
CLRFLD   SR    R3,R3               CLEAR OPTIONAL FIELDS                        
         IC    R3,FLDLEN                                                        
         SH    R3,=H'9'                                                         
         EX    R3,*+8                                                           
         B     *+14                                                             
         XC    FLDDATA,FLDDATA     * EXECUTED *                                 
         FOUT  (R2)                                                             
         BR    R9                                                               
         SPACE 1                                                                
* STANDARD PROGRAM CALL *                                                       
         SPACE 1                                                                
GETOV    GOTO1 VCALLOV,DMCB,(BYTE,0),(RA)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RC,DMCB                                                          
         BASR  RE,RF                                                            
         BR    R9                                                               
         TITLE 'DEMO INFORMATION SYSTEEN - FIELD EDIT ROUTINES'                 
* EDIT STATION CALL LETTERS                                                     
*                                                                               
EDTSTA   LA    R3,INVSTA                                                        
         CLI   FLDILEN,3                                                        
         BL    ERROR                                                            
         CLI   SCRENNO,X'FC'                                                    
         BNE   STAREQ              ALL VALID FOR SCREEN FC                      
         CLC   FLDDATA(3),=C'ALL'                                               
         BNE   *+12                                                             
         XC    STAT,STAT                                                        
         BR    R9                                                               
STAREQ   MVC   STAT,=C'     '                                                   
         BAS   RE,MOVE                                                          
         MVC   STAT,WORK                                                        
         MVI   STAT+4,C'T'                                                      
         CLC   STAT,PREVSTA                                                     
         BER   R9                                                               
         MVC   PREVSTA,STAT                                                     
         BAS   R3,CLRPREV          RESET EXPLODE IF NEW STATION                 
* TEST FOR NUMERIC STATION (MARKET TOTAL REQUEST)                               
         TM    FLDIIND,X'08'                                                    
         BZ    STA2                                                             
         BAS   RE,PACK                                                          
         STH   R0,AGYMKT                                                        
         BR    R9                                                               
*                                                                               
STA2     MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),CMEDIA                                                  
         MVC   KEY+2(5),PREVSTA                                                 
         MVC   KEY+7(2),AGYALPHA                                                
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,IOAREA                  
         TM    8(R1),X'50'                                                      
         BZ    STAOK                                                            
         LA    R3,INVSTA                                                        
         B     ERROR                                                            
STAOK    PACK  DUB,IOAREA+18(4)                                                 
         CVB   R3,DUB                                                           
         STH   R3,AGYMKT                                                        
         BR    R9                                                               
         EJECT                                                                  
* EDIT BOO*                                                                     
         SPACE 1                                                                
EDTBOOK  GOTO1 VDATVAL,DUB,(X'02',FLDDATA),WORK                                 
         LA    R3,INVDATE                                                       
         CLI   DUB+3,0                                                          
         BE    ERROR2                                                           
         SR    RE,RE                                                            
         MVI   WORK,C'0'                                                        
         PACK  DUB,WORK(2)                                                      
         CVB   RE,DUB                                                           
         SLL   RE,4                                                             
         STC   RE,B1                                                            
         PACK  DUB,WORK+2(2)                                                    
         CVB   RE,DUB                                                           
         STC   RE,DUB                                                           
         STC   RE,B2               SET ADJ TO BOOK MONTH                        
         OC    B1,DUB                                                           
         OC    STAT,STAT                                                        
         BZR   R9                                                               
*                                                                               
* CHECK FOR BOOK ON FILE *                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),CMEDIA                                                  
         MVI   KEY+2,C'A'                                                       
         CLI   CSOURCE,1                                                        
         BE    *+8                                                              
         MVI   KEY+2,C'N'                                                       
         MVC   KEY+3(5),STAT                                                    
         MVC   BYTE2,B1                                                         
         NI    BYTE2,X'0F'                                                      
         SR    RF,RF                                                            
         IC    RF,B1                                                            
         SRL   RF,4                                                             
         LA    RE,70                                                            
         CH    RF,=H'5'                                                         
         BH    *+8                                                              
         LA    RE,80                                                            
         AR    RE,RF                                                            
         STC   RE,BYTE                                                          
         XC    BYTE(2),=X'FFFF'                                                 
         MVC   KEY+8(2),BYTE                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',KEY,IOAREA                   
         CLC   KEY(10),IOAREA                                                   
         BNE   EDTBOOK1                                                         
         CLC   B1,PREVB1                                                        
         BER   R9                                                               
         MVC   PREVB1,B1           RESET EXPLODE IF BOOK CHANGE                 
         BAS   R3,CLRPREV                                                       
         BR    R9                                                               
EDTBOOK1 XC    DEMMSG,DEMMSG                                                    
         MVC   DEMMSG(16),=C'BOOK NOT ON FILE'                                  
         B     EXIT                                                             
         EJECT                                                                  
* EDIT HUT *                                                                    
*                                                                               
EDTHUT   CLI   FLDILEN,0                                                        
         BER   R9                                                               
         MVC   WORK+6(3),FLDDATA                                                
         MVC   WORK+9(3),=C'/75'                                                
         GOTO1 VDATVAL,DUB,(X'02',WORK+6),WORK                                  
         LA   R3,INVDATE                                                        
         CLI   DUB+3,0                                                          
         BE    ERROR                                                            
         PACK  DUB,WORK+2(2)                                                    
         CVB   RE,DUB                                                           
         STC   RE,B2                                                            
         CLC   PREVB2,B2                                                        
         BER   R9                                                               
         MVC   PREVB2,B2                                                        
         BAS   R3,CLRPREV          RESET EXPLODE IF HUT CHANGE                  
         BR    R9                                                               
         SPACE 2                                                                
* EDIT MARKET *                                                                 
         SPACE 1                                                                
EDTMAR   MVI   SPILLSW,0                                                        
         XC    SPLIST,SPLIST                                                    
         CLI   CMEDIA,C'C'                                                      
         BNER  R9                                                               
         CLI   FLDILEN,0                                                        
         BER   R9                                                               
         ZIC   R3,FLDILEN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         OC    FLDDATA(0),FLDDATA                                               
         BZR   R9                                                               
         LA    R3,INVDAY                                                        
         CLI   FLDILEN,3                                                        
         BNE   EDTMAR3                                                          
         MVI   SPILLSW,3                                                        
         CLC   FLDDATA(3),=C'ALL'                                               
         BER   R9                                                               
EDTMAR3  MVI   SPILLSW,2                                                        
         BAS   RE,PACK                                                          
         STH   R0,SPLIST                                                        
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
*        EDIT DEMO NUMBERS                                                      
*                                                                               
EDTDEM1  XC    DEMLST,DEMLST       INITIALIZE DEMO LISTS                        
         CLC   DEMMSG(5),=C'ENTER'                                              
         BE    EXIT                                                             
         XC    DEMNAME,DEMNAME                                                  
         XC    DYTMLST,DYTMLST                                                  
         LA    R6,WORKEND                                                       
         A     R6,=F'2700'                                                      
         XC    0(255,R6),0(R6)                                                  
         USING DBLOCK,R6                                                        
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CMEDIA                                                  
         L     R3,VCOMFACS                                                      
         USING COMFACSD,R3                                                      
         GOTO1 CDEMOVAL,DMCB,(7,(R2)),(7,DEMLST),(C'S',(R6)),0                  
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(7,DEMLST),(2,DEMNAME),(C'S',(R6)),0                   
         DROP  R3                                                               
         LA    R3,INPMIS                                                        
         BR    R9                                                               
         EJECT                                                                  
CLRPREV  XC    SCRNPAGE(2),SCRNPAGE                                             
         XC    SAVDYTM,SAVDYTM                                                  
         BR    R3                                                               
*                                                                               
EDTPAGE  MVI   SCRNPAGE,1                                                       
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZR   R9                                                               
         STC   R0,SCRNPAGE                                                      
         BR    R9                                                               
*                                                                               
*        TABLE FORMAT = FUNCTION NUMBER                                         
*                        EDIT FIELDS                                            
*                         X'01' = STATION                                       
*                         X'02' = BOOK                                          
*                         X'04' = HUT                                           
*                         X'08' = DEMOES                                        
*                         X'01' = DAY/TIME                                      
*                        PROGRAM OVERLAY NUMBER                                 
*                        SCREEN OVERLAY NUMBER                                  
*                                                                               
ACTTAB   DC    X'010001FC'                                                      
         DC    C'DLI'         LIST DEMO CODES AND DESCRIPTIONS                  
         DC    X'010002FC'    2                                                 
         DC    C'ALI'         LIST ADJUSTMENT AND DEMOS APPLIED TO              
         DC    X'020305FC'    3                                                 
         DC    C'SLI'         LIST ACTIVE STATIONS                              
         DC    X'010104FC'    4                                                 
         DC    C'BLI'         LIST BOOKS ON FILE                                
         DC    X'010305FC'    5                                                 
         DC    C'MLI'         LIST MARKETS ON FILE                              
         DC    X'001B06FC'    6                                                 
         DC    C'PLI'         LIST PROGRAM NAMES                                
         DC    X'023F07FE'                                                      
         DC    C'DEX'              EXPLODE DEMOS                                
         DC    X'000108FD'    8                                                 
         DC    C'ADI'         DISPLAY ADJUSTMENT FACTORS                        
         DC    X'013F07FE'                                                      
         DC    C'DCO'         COMPUTE DEMOS                                     
         DC    X'033F07FE'                                                      
         DC    C'ACO'                                                           
         DC    X'043F07FE'                                                      
         DC    C'AEX'                                                           
         DC    A(0)           END OF TABLE                                      
ERROR2   B     ERROR                                                            
         EJECT                                                                  
INVRSRC  EQU   2                                                                
INVSTA   EQU   2                                                                
INVDATE  EQU   2                                                                
DEMREQ   EQU   1                                                                
INVDEM   EQU   2                                                                
INVDAY   EQU   2                                                                
INVTIME  EQU   2                                                                
INPMIS   EQU   1                                                                
INVFMT   EQU   5                                                                
INVACT   EQU   12                                                               
         EJECT                                                                  
       ++INCLUDE GENEROL                                                        
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE GENOLD                                                         
       ++INCLUDE SPGENBUY                                                       
         ORG   IOAREA                                                           
       ++INCLUDE T20EWORK                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039T20E00    05/01/02'                                      
         END                                                                    
