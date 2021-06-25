*          DATA SET RECNT28    AT LEVEL 012 AS OF 11/29/00                      
*PHASE T80228A,+0                                                               
*INCLUDE OUTDAY                                                                 
*INCLUDE REGENBUF                                                               
*INCLUDE REGENPBR                                                               
*INCLUDE REGENTL2                                                               
*INCLUDE UNTIME                                                                 
         TITLE 'T80228 - REPPAK MULTIPLE BUY DISPLAY: RTS INFO'                 
***********************************************************************         
*                                                                     *         
*  RECNT28 -- MULTIPLE BUY DISPLAY: REP-TO-SPOT INFO                  *         
*                                                                     *         
*  HISTORY:                                                           *         
*                                                                     *         
*  05/05/93 (BU ) --- ORIGINAL ENTRY                                  *         
*                                                                     *         
*  10/06/95 (SKU) --- 2K CONTRACT SUPPORT                             *         
***********************************************************************         
T80228   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80228,RR=R5                                                   
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=H'4096'         4K                                           
         USING TWAWORK,R7                                                       
* FOUT AND CLEAR NON-ZERO LINES                                                 
M5       LA    R2,RTSLIN1H                                                      
         LA    R3,RTSLAST-9                                                     
         SR    RE,RE                                                            
*                                                                               
M10      OC    8(79,R2),8(R2)                                                   
         BZ    M15                                                              
         XC    8(79,R2),8(R2)                                                   
         FOUT  (R2)                                                             
M15      IC    RE,0(R2)            FIELD LEN                                    
         LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3                                                            
         BL    M10                                                              
*                                                                               
M30      MVC   KEY+28(4),TWAKADDR  K DISK ADDR                                  
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RCONREC                                     
         L     RE,=V(OUTDAY)                                                    
         AR    RE,R5                                                            
         ST    RE,DMCB2                                                         
         L     RE,=V(UNTIME)                                                    
         AR    RE,R5                                                            
         ST    RE,DMCB2+4                                                       
         MVC   DMCB2+8(4),DATCON                                                
         SR    R6,R6               LINE CTR                                     
         LA    R2,RTSLIN1H                                                      
         CLC   CONBNUM(3),=C'TOT'  TOTALS ONLY?                                 
         BE    TOTAL                                                            
         CLC   CONBNUM(3),=C'CHK'  SPECIAL TOTALS DISPLAY                       
         BNE   M34                                                              
         CLC   REPALPHA,=C'TO'     FOR TORBET ONLY                              
         BNE   M33                                                              
         CLC   RCONKCON,=X'00218689' CONTRACT NO. FROM 0 TO 218689              
         BH    M33                                                              
         XC    WORK3,WORK3         CLEAR OUT WORK AREA                          
         B     M34                                                              
M33      LA    R3,12               INVALID ACTION                               
         LA    R2,CONBNUMH         POINT TO BUY NUMBER FIELD                    
         B     ERROR                                                            
*                                                                               
M34      EQU   *                                                                
         XC    RBUYKEY,RBUYKEY     CLEAR KEY                                    
         MVI   RBUYKTYP,X'0B'      BUILD BUY KEY                                
         MVC   RBUYKREP,REPALPHA   REP                                          
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
*                                                                               
         LA    R2,CONBNUMH                                                      
         MVI   BYFLT,0             ZERO FLIGHT NUMBER                           
         CLI   CONBNUM,C'F'        TEST FOR FLIGHT NUMBER FILTER PREFIX         
         BNE   M35                 NOT THERE                                    
         SPACE                                                                  
         LA    R3,INVINP                                                        
         CLI   5(R2),1             TEST INPUT LENGTH FOR UP TO                  
         BE    ERROR               2 DIGIT FLIGHT NUMBER                        
         CLI   5(R2),3                                                          
         BH    ERROR                                                            
         SPACE                                                                  
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         MVC   WORK(3),=3X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),CONBNUM+1                                                
         CLC   WORK(3),=3X'F0'                                                  
         BL    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CONBNUM+1(0)                                                 
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERROR               ZERO IS INVALID FLIGHT NUMBER                
         STC   R0,BYFLT                                                         
         B     M35                                                              
         SPACE                                                                  
M35      GOTO1 VPACK                                                            
*                                                                               
         STC   R0,STARTLIN         STARTING LINE NUMBER                         
*                                                                               
         LA    R2,RTSLIN1H                                                      
         CLC   CONBNUM(4),=C'NEXT'                                              
         BE    M60                                                              
M50      XC    TWALSTKY,TWALSTKY                                                
         XC    TWALSTPL,TWALSTPL                                                
         B     M100                                                             
*                                                                               
* NEXT PAGE REQUESTED                                                           
M60      CLC   TWALSTKY(22),RBUYKEY     SAME K?                                 
         BNE   M50                                                              
         MVC   RBUYKEY+22(5),TWALSTKY+22                                        
         SR    R4,R4                                                            
         IC    R4,RBUYKEY+26                                                    
         LA    R4,1(R4)                                                         
         CH    R4,=H'255'                                                       
         BH    *+8                                                              
         STC   R4,RBUYKEY+26       FOR READ HIGH TO NEXT KEY                    
*                                                                               
M100     MVC   KEY,RBUYKEY                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         B     NEXTBUY1                                                         
         SPACE 1                                                                
NEXTBUY  OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
NEXTBUY1 CLC   KEY(22),KEYSAVE     SAME K?                                      
         BNE   TOTAL                                                            
         TM    KEY+27,X'C0'        VOID?                                        
         BO    NEXTBUY                                                          
         CLC   CONBNUM(4),=C'NEXT'                                              
         BE    M200                                                             
*                                                                               
         CLI   STARTLIN,0          STARTING LINE # GIVEN?                       
         BE    M200                                                             
*                                                                               
         CLC   KEY+26(1),STARTLIN                                               
         BNE   NEXTBUY                                                          
         MVI   STARTLIN,0          DON'T USE AGAIN                              
*                                                                               
* GET BUY RECORD                                                                
M200     OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
*                                                                               
*   FOR NEW 'COMBO' FACILITY, A BUY ACTION OF 'DSS' CAN FORCE                   
*     A COMBO ORDER TO ENTER THIS MODULE FOR THE DISPLAY OF A                   
*     SINGLE STATION'S PORTION OF THAT ORDER.  IF SUCH AN ORDER                 
*     IS REQUESTED, 'NA' BUYLINES SHOULD NOT - REPEAT, NOT - BE                 
*     DISPLAYED ON THIS SCREEN.                                                 
*                                                                               
         TM    RBUYCOMB,X'80'      IS 'NA' FLAG SET FOR BUYLINE?                
         BO    NEXTBUY             YES - SKIP THIS BUYLINE                      
         CLI   BYFLT,0             TEST FOR FLIGHT FILTER                       
         BE    *+14                NONE                                         
         CLC   RBUYFLT,BYFLT       DOES BUY BELONG TO FLIGHT                    
         BNE   NEXTBUY             NO                                           
* DISPLAY BUY                                                                   
         XC    DMCB2+12(4),DMCB2+12   NO ALT WEEK EXPLOSION                     
         GOTO1 =V(REGENPBR),DMCB,RBUYREC,(10,AIO4),DMCB2,RCONREC,RR=Y           
         CLI   DMCB+4,0            NO LINES?                                    
         BE    NEXTBUY                                                          
* DISPLAY LINE                                                                  
*                                                                               
M400     CLC   CONBNUM(3),=C'CHK'  SPECIAL TOTALS DISPLAY                       
         BE    M600                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DMCB+4           NUMBER OF BUY DISPLAY LINES                  
         AR    R6,RE                                                            
         CH    R6,=H'12'           MAX                                          
         BH    XIT1                                                             
*                                                                               
         L     R8,AIO4                                                          
M500     MVC   8(79,R2),0(R8)                                                   
         FOUT  (R2)                                                             
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         LA    R2,0(R4,R2)         NEXT FIELD                                   
         LA    R8,L'PRTLN(R8)                                                   
         BCT   RE,M500             LOOP TO DISPLAY BUY LINES                    
* SAVE BUYKEY                                                                   
         MVC   TWALSTKY,KEY                                                     
         B     NEXTBUY                                                          
         SPACE 3                                                                
* CHK - SPECIAL TOTALS DISPLAY                                                  
         SPACE 1                                                                
M600     TM    RBUYCNTL,X'80'      CANCELLED OR DELETED                         
         BO    NEXTBUY             DON'T ADD INTO TOTALS                        
         SPACE 1                                                                
         XC    WORK2,WORK2                                                      
         MVC   WORK(4),VGTBROAD                                                 
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
         SPACE 1                                                                
         GOTO1 REGENBUC,DMCB,RBUYREC,WORK2,WORK,                                
         SPACE 1                                                                
         CLC   WORK2(2),=H'2'      NO DATA                                      
         BE    NEXTBUY                                                          
         SR    R0,R0                                                            
* ADD BUY BUCKETS TO TOTAL CONTRACT BUCKETS                                     
         LA    R3,WORK2+2          FIRST BUCKET                                 
M610     LA    R4,WORK3+2          TOTAL                                        
*                                                                               
M620     CLI   0(R4),0             LAST?                                        
         BE    M630                                                             
         CLC   2(2,R3),2(R4)       COMPARE BUCKET DATES                         
         BL    M630                                                             
         BE    M650                                                             
* GET NEXT TOTAL BUCKET                                                         
         IC    R0,1(R4)            LEN                                          
         AR    R4,R0                                                            
         B     M620                                                             
*                                                                               
* ADD NEW TOTAL BUCKET                                                          
M630     GOTO1 VRECUP,DMCB,(X'FF',WORK3),(R3),(R4)                              
*                                                                               
* GET NEXT BUY BUCKET                                                           
M640     IC    R0,1(R3)            LENGTH                                       
         AR    R3,R0                                                            
         CLI   0(R3),0             LAST?                                        
         BNE   M610                                                             
         MVC   TWALSTKY,KEY                                                     
         B     NEXTBUY                                                          
         SPACE 1                                                                
* ADD TO TOTAL BUCKET                                                           
M650     MVC   DUB(8),6(R3)                                                     
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R4)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R4),DUB                                                      
         B     M640                                                             
         SPACE 2                                                                
         EJECT                                                                  
*              DISPLAY TOTALS - GET CONTRACT                                    
TOTAL    DS    0H                                                               
         CLC   CONBNUM(3),=C'CHK'  SPECIAL TOTALS ACTION                        
         BNE   TOT20                                                            
*                                                                               
         MVC   8(16,R2),=C'*BUYLINE TOTALS*'                                    
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         OC    WORK3,WORK3         ANY BUYS                                     
         BNZ   TOT5                                                             
         MVC   68(15,R2),=C'TOTAL      $.00'                                    
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     TOT20                                                            
         SPACE 1                                                                
TOT5     GOTO1 =V(REGENTL2),DMCB,WORK3,AIO4,RR=YES                              
*                                                                               
*  MOVE BUYLINE TOTALS TO SCREEN                                                
         L     R8,AIO4                                                          
         SR    RE,RE                                                            
         IC    RE,DMCB+4           NUMBER OF TOTAL LINES                        
*                                                                               
TOT10    MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         FOUT  (R2)                                                             
         LA    R8,80(R8)                                                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,TOT10                                                         
         SPACE 1                                                                
TOT20    GOTO1 =V(REGENBUF),DMCB,RCONREC,WORK2,RR=YES                           
*                                                                               
* REGENBUF BUILDS MONTHLY BUCKETS FROM CONTRACT REC FOR REGENTL2                
         GOTO1 =V(REGENTL2),(R1),WORK2,AIO4,RR=YES                              
*                                                                               
         ZIC   RE,DMCB+4           NUMBER OF TOTAL LINES                        
         AR    R6,RE                                                            
         CH    R6,=H'12'                                                        
         BH    XIT1                                                             
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         LA    RE,1                                                             
*              MOVE BUCKET TOTALS TO SCREEN                                     
         L     R8,AIO4                                                          
*                                                                               
         CLC   CONBNUM(3),=C'CHK'        SPECIAL TOTAL DISPLAY                  
         BNE   TOT100                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(18,R2),=C'*CONVERTED TOTALS*'                                  
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
TOT100   MVC   8(79,R2),0(R8)      TOTAL LINE                                   
         FOUT  (R2)                                                             
         LA    R8,80(R8)                                                        
         ZIC   R0,0(R2)            NEXT DISPLAY LINE                            
         AR    R2,R0                                                            
         BCT   RE,TOT100                                                        
         EJECT                                                                  
*              DISPLAY TOTALS                                                   
XIT      LA    R2,CONBACTH         CURSOR                                       
         XC    DMCB(24),DMCB                                                    
         SR    R3,R3                                                            
         MVI   DMCB+3,53                                                        
         CLI   BYFLT,0             HAVE BUYS BEEN FILTERED BY FLIGHT            
         BE    XIT2                NO                                           
         MVI   DMCB+3,54                                                        
         EDIT  (B1,BYFLT),(3,TEMP),ALIGN=LEFT                                   
         LR    R3,R0                                                            
         B     XITA                                                             
*                                                                               
XIT2     CLC   CONBNUM(3),=C'CHK'  SPECIAL TOTALS DISPLAY                       
         BE    XIT4                                                             
         CLC   CONBNUM(3),=C'TOT'   TOTALS ONLY?                                
         BNE   XITA                                                             
XIT4     MVI   DMCB+3,55                                                        
XITA     FOUT  CONBNUMH,MYSPACES,8                                              
XITE     OI    CONBACTH+4,X'20'                                                 
         OI    CONBNUMH+4,X'20'                                                 
         LTR   R3,R3                                                            
         BZ    XITF                                                             
         GOTO1 VDISMSG,DMCB,,0,0,((R3),TEMP)                                    
         B     EXIT                                                             
XITF     GOTO1 VDISMSG,DMCB,,                                                   
         B     EXIT                                                             
*                                                                               
XIT1     DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         SR    R3,R3                                                            
         NI    CONBACTH+6,X'BF'    NO CURSOR                                    
         MVI   DMCB+3,56            HIT ENTER FOR NEXT SCRN                     
         CLI   BYFLT,0             DISPLAY THE FLIGHT FILTER IF ANY             
         BE    XITK                NONE                                         
         MVI   DMCB+3,57            HIT ENTER FOR NEXT SCRN - FLIGHT NN         
         EDIT  (B1,BYFLT),(3,TEMP),ALIGN=LEFT                                   
         LR    R3,R0                                                            
XITK     LA    R2,CONBNUMH                                                      
         MVC   8(4,R2),=C'NEXT'                                                 
         OI    1(R2),X'01'         TURN ON MODIFIED FOR AUTO PAGING             
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         B     XITE                                                             
         SPACE 2                                                                
STARTLIN DS    CL1                 1 BYTE BINARY STARTING LINE #                
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTE7D                                                       
       ++INCLUDE REGENPBRD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012RECNT28   11/29/00'                                      
         END                                                                    
