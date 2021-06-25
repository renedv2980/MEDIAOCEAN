*          DATA SET CTDEB01    AT LEVEL 005 AS OF 03/23/15                      
*PROCESS USING(WARN(15))                                                        
*&&      SET   NOP=N                                                            
*PHASE TA0F01A                                                                  
*INCLUDE DEBDIS                                                                 
         TITLE 'CTDEB01 - DEBUG MODULE'                                         
CTDEB01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DB1**,RA,RR=R4                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         ST    RD,PHASERD                                                       
         ST    R4,RELO1                                                         
         USING CTDEBFFD,R8         R8=A(TWA)                                    
         USING SAVED,R7            R7=A(SAVED)                                  
*                                                                               
         BAS   RE,MAIN                                                          
*                                                                               
XMOD1    L     RD,PHASERD                                                       
         XMOD1 1                   EXIT PROGRAM                                 
*                                                                               
XITEQU   CR    RB,RB               EXIT CC EQU                                  
         B     XIT1                                                             
XITNEQ   LTR   RB,RB               EXIT CC NEQ                                  
*                                                                               
XIT1     XIT1                      EXIT                                         
*                                                                               
AREGON   CLI   ADDFLAG,0           CHECK FOR DATASPACE                          
         BER   RE                                                               
         SAC   512                                                              
         LAM   AR2,AR2,DSPALET                                                  
         LAM   ARE,AR1,ZEROS                                                    
         BR    RE                                                               
AREGOFF  CLI   ADDFLAG,0           CHECK FOR DATASPACE                          
         BER   RE                                                               
         SAC   0                                                                
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM                                       *                   
*************************************************************                   
MAIN     NTR1                                                                   
*                                                                               
         CLI   SUBACT,2            TEST LOCATE HEXFLD                           
         BNE   MAIN010                                                          
         OC    HEXFLD,HEXFLD                                                    
         BZ    MAIN001                                                          
         MVC   ADDRESS,HEXFLD                                                   
         XC    HEXFLD,HEXFLD                                                    
         B     MAIN030                                                          
*                                                                               
MAIN001  CLC   INF01(3),=C'PSW'    TEST PSW LOCATE                              
         BNE   MAIN005                                                          
         OI    FLAG1,FLOCPSW       FLAG PSW LOCATE PENDING                      
         B     MAIN030                                                          
*                                                                               
MAIN005  GOTO1 ALOCATE             OR LOCATE LABEL                              
         L     R1,ADDRESS                                                       
MAIN007  CLI   INFCHR,C'+'         + OR -                                       
         BNE   *+8                                                              
         A     R1,DECFLD           DECIMAL DISP                                 
         CLI   INFCHR,C'-'                                                      
         BNE   *+8                                                              
         S     R1,DECFLD                                                        
         ST    R1,ADDRESS                                                       
         B     MAIN030                                                          
*                                                                               
MAIN010  CLI   PFKEY,5             TEST FIND PF KEY                             
         BE    *+12                                                             
         CLI   SUBACT,1            TEST FIND                                    
         BNE   MAIN020                                                          
         CLI   ACTION,6            LIST FIND                                    
         BNE   MAIN015                                                          
         BAS   RE,FINDWRK          SEARCH WORKER FILE                           
         BE    MAIN030                                                          
         BNE   MAINX                                                            
*                                                                               
MAIN015  BAS   RE,CHKADDR                                                       
         BAS   RE,FINDSTR          SEARCH CORE                                  
         B     MAIN030                                                          
*                                                                               
MAIN020  BAS   RE,CHECKPF          ADJUST FOR PF KEYS                           
         NI    PAGEFLG,255-PAGENDQ                                              
*                                                                               
MAIN030  BAS   RE,CHKADDR                                                       
*                                                                               
MAIN040  BAS   RE,SETSIZE          SET SIZE AND SCROLL                          
*                                                                               
MAIN050  CLI   ACTION,3            PATCH                                        
         BE    MAIN090                                                          
         CLI   ACTION,4            DISPLAY                                      
         BNE   *+14                                                             
         XC    BASEADDR,BASEADDR                                                
         B     MAIN090                                                          
         CLI   ACTION,5            DISS                                         
         BE    MAIN080                                                          
         CLI   ACTION,6            LIST                                         
         BE    MAIN100                                                          
*                                                                               
MAIN080  LA    R1,DEBTABH          ACTION DISS                                  
         BAS   RE,CHKCODE                                                       
         BAS   RE,CHKWATCH                                                      
         LA    R1,DEBTABH                                                       
         BAS   RE,BLDCODE                                                       
         BAS   RE,FILLDIS                                                       
         BAS   RE,BLDWATCH                                                      
         B     MAINX                                                            
*                                                                               
MAIN090  LA    R1,DEBTABH          ACTION DISPLAY                               
         BAS   RE,CHKHEXS                                                       
         BAS   RE,CHKADDR          CHECK ADDRESS BOUNDARYS                      
         LA    R1,DEBTABH                                                       
         BAS   RE,BLDHEXS                                                       
         BAS   RE,FILLHEX                                                       
         BAS   RE,BLDWATCH                                                      
         B     MAINX                                                            
*                                                                               
MAIN100  EQU   *                                                                
         BAS   RE,GETWRKF                                                       
         BNE   MAINX               ERROR NOT FOUND                              
         LA    R1,DEBTABH                                                       
         MVC   BASEADDR,ADDRESS                                                 
         BAS   RE,CHKCODE                                                       
         BAS   RE,CHKWATCH                                                      
         LA    R1,DEBTABH                                                       
         BAS   RE,BLDCODE                                                       
         BAS   RE,BLDWATCH                                                      
         BAS   RE,FILLCOD                                                       
         B     MAINX                                                            
*                                                                               
MAINX    TM    FLAG1,FLOCPSW       STILL LOOKING ARE WE                         
         BZ    *+12                                                             
         NI    FLAG1,255-FLOCPSW                                                
         B     MAIN005                                                          
         BAS   RE,AREGOFF          BE SURE ITS OFF                              
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        BUILD HEX DISPLAY SCREEN                           *                   
*************************************************************                   
         SPACE 1                                                                
BLDHEXS  ST    RE,SAVERE           NO NTR1 (DESTROYS R0,R1,RE,RF)               
         OI    DEBMSGH+6,X'80'                                                  
         OI    DEBSRVH+6,X'80'                                                  
         OI    DEBAC1H+6,X'80'                                                  
         OI    DEBACTH+6,X'80'                                                  
         MVC   INSCREEN,=X'FF01'                                                
         SR    RE,RE                                                            
         LH    R0,SIZE             MAX 22                                       
         ICM   RE,3,2(R1)          SAVE SCREEN ADDR                             
*                                                                               
BLDHX0   MVC   0(16,R1),HEXFLD1    MOVE IN 8 BYTES PROT                         
         STCM  RE,3,2(R1)                                                       
         LA    RE,9(RE)                                                         
         LA    R1,16(R1)                                                        
         LA    RF,4                THEN FOUR TIMES                              
*                                                                               
BLDHX1   MVC   0(9,R1),HEXFLD2     1 BYTE UNPROT                                
         STCM  RE,3,2(R1)                                                       
         LA    RE,2(RE)                                                         
         LA    R1,9(R1)                                                         
         MVC   0(16,R1),HEXFLD1    8 BYTES PROT                                 
         CLI   ACTION,3            UNPROT FOR PATCH                             
         BNE   *+8                                                              
         NI    1(R1),255-X'20'                                                  
         STCM  RE,3,2(R1)                                                       
         LA    RE,9(RE)                                                         
         LA    R1,16(R1)                                                        
         BCT   RF,BLDHX1           FOUR TIMES DONE                              
*                                                                               
         LA    RE,6(RE)                                                         
         MVC   0(24,R1),HEXFLD3    16 BYTE CHR FIELD                            
         CLI   ACTION,3            UNPROT FOR PATCH                             
         BNE   *+8                                                              
         NI    1(R1),255-X'20'                                                  
         STCM  RE,3,2(R1)                                                       
         LA    RE,21(RE)                                                        
         LA    R1,24(R1)                                                        
         BCT   R0,BLDHX0           22 TIMES DONE                                
*                                                                               
         MVC   0(3,R1),=X'000101'  NEW END OF SCREEN                            
BLDHXX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        CHECK CODE SCREEN                                  *                   
*************************************************************                   
         SPACE 1                                                                
CHKCODE  NTR1                                                                   
         CLI   PFKEY,9             DON'T CHECK IF RESIZING                      
         BE    CHKCXX                                                           
         LR    R3,R1                                                            
         XC    FULL1,FULL1         SET FULL1 TO PSW ADDR                        
         ICM   R1,15,DBTCB                                                      
         BZ    *+16                                                             
         MVC   FULL1,TCBPSW+4-TCBD(R1)                                          
         NC    FULL1,=X'7FFFFFFF'                                               
*                                                                               
         CLC   INSCREEN,=X'FF02'                                                
         BNE   CHKCXX                                                           
         LH    R0,SIZE             MAX 22                                       
CHKC001  CLC   8(4,R3),=C'PSW='                                                 
         BNE   CHKC002                                                          
         MVC   FULL,FULL1                                                       
         B     CHKC009                                                          
CHKC002  CLC   8(6,R3),=C'PSW- 2'                                               
         BNE   CHKC008                                                          
         L     R1,FULL1                                                         
         SH    R1,=H'2'                                                         
         ST    R1,FULL                                                          
         B     CHKC009                                                          
*                                                                               
CHKC008  GOTO1 AHEXIN,DMCB,(0,8(R3)),FULL,8                                     
*NOP     CLI   ACTION,5                                                         
*NOP     BE    CHKC009                                                          
*                                                                               
         L     R1,FULL                                                          
         ICM   RF,15,BASEADDR                                                   
*        BNZ   *+8                                                              
*        A     R1,ADDRESS                                                       
         AR    R1,RF                                                            
         ST    R1,FULL                                                          
*                                                                               
CHKC009  SAM31                                                                  
         BAS   RE,AREGON                                                        
         L     R2,FULL             R2=ADDR                                      
         LA    R3,16(R3)                                                        
*                                                                               
CHKC010  TM    4(R3),X'80'         ANY SEL INPUT                                
         BZ    CHKC090                                                          
*                                                                               
         CLI   8(R3),C'R'          R FOR RESET BREAKPOINT                       
         BE    RSETBRK                                                          
         CLC   8(5,R3),=C'BREAK'   BREAKPOINT                                   
         BE    CHKC090                                                          
         CLI   8(R3),C'X'          X FOR BREAKPOINT                             
         BE    CHKCBRK                                                          
*                                                                               
         CLI   8(R3),C'W'          WATCH THIS                                   
         BE    CHKC020                                                          
         CLI   8(R3),C'?'          ? OR G                                       
         BE    CHKC021                                                          
         CLI   8(R3),C'G'                                                       
         BE    CHKC021                                                          
         CLI   8(R3),C'P'          P FOR SET PSW                                
         BNE   CHKC022                                                          
         ICM   R1,15,DBTCB         SET PSWADR IN DEBUG TCB                      
         BZ    CHKC090                                                          
         MVC   TCBPSW+4-TCBD(4,R1),FULL                                         
         B     CHKC090                                                          
*                                                                               
CHKCBRK  L     RF,AMYEXIT          SET A BREAKPOINT                             
         LA    RF,EXBREAK-EXAREAD(RF)                                           
         LA    R1,64                                                            
*                                                                               
CHKB01   OC    0(4,RF),0(RF)       FREE ENTRY                                   
         BZ    CHKB02                                                           
         C     R2,0(RF)            ALREADY SET                                  
         BE    CHKB02                                                           
         LA    RF,4(RF)            TRY NEXT                                     
         BCT   R1,CHKB01                                                        
         DC    H'0'                MESSAGE TOO MANY BREAKPOINTS                 
*                                                                               
CHKB02   ST    R2,0(RF)            SAVE BREAK ADDRESS                           
         B     CHKC090                                                          
*                                                                               
RSETBRK  L     RF,AMYEXIT          CHECK BREAKS                                 
         LA    RF,EXBREAK-EXAREAD(RF)                                           
         LA    R1,64                                                            
*                                                                               
RSEB01   C     R2,0(RF)   ??? DO DESTRCTIVE MOVE UP TO REMOVE ???               
         BE    RSEB02                                                           
         LA    RF,4(RF)                                                         
         BCT   R1,RSEB01                                                        
         MVC   0(2,R2),=X'0002'    SPECIAL CODE TO RESTORE FROM CSECT           
         B     CHKC090                                                          
*                                                                               
RSEB02   XC    0(4,RF),4(RF)       ZAP IT                                       
         SHI   R1,1                                                             
         BZ    CHKC090                                                          
*                                                                               
RSEB03   MVC   0(4,RF),4(RF)       BUMP OTHER ENTRIES UP ONE                    
         LA    RF,4(RF)                                                         
         BCT   R1,RSEB03                                                        
         XC    0(4,RF),0(RF)       ZAP LAST ENTRY                               
         B     CHKC090                                                          
*                                                                               
CHKC020  LA    R1,WATCHES          WATCH THIS FIELD                             
         LA    RF,WATCHEX                                                       
CHKC020A OC    0(16,R1),0(R1)                                                   
         BZ    CHKC020B                                                         
         LA    R1,16(R1)                                                        
         CR    R1,RF                                                            
         BL    CHKC020A                                                         
         B     CHKC090                                                          
*                                                                               
CHKC020B MVC   0(8,R1),36(R3)      SET LABEL                                    
         ST    R2,8(R1)            SET ADDRESS                                  
         MVI   12(R1),C'X'         SET TO HEX DISPLAY                           
         MVI   13(R1),12           SET TO LENGTH 12                             
         MVI   14(R1),C'A'         SET TO ABSOLUTE                              
         CLC   8(3,R3),=C'WCL'                                                  
         BE    CHKC020C                                                         
         CLC   8(3,R3),=C'WXL'                                                  
         BE    CHKC020D                                                         
         CLI   45(R3),C'D'                                                      
         BNE   CHKC090                                                          
*                                                                               
         CLC   51(2,R3),=C'XL'                                                  
         BNE   *+14                                                             
         MVC   9(5,R3),51(R3)                                                   
         B     CHKC020D                                                         
*                                                                               
         CLC   51(2,R3),=C'CL'                                                  
         BNE   *+14                                                             
         MVC   9(5,R3),51(R3)                                                   
         B     CHKC020C                                                         
*                                                                               
         CLC   51(1,R3),=C'H'                                                   
         BNE   *+14                                                             
         MVC   9(5,R3),=C'XL2  '                                                
         B     CHKC020D                                                         
*                                                                               
         CLC   51(1,R3),=C'F'                                                   
         BNE   *+14                                                             
         MVC   9(5,R3),=C'XL4  '                                                
         B     CHKC020D                                                         
*                                                                               
         CLC   51(1,R3),=C'A'                                                   
         BNE   *+14                                                             
         MVC   9(5,R3),=C'XL4  '                                                
         B     CHKC020D                                                         
*                                                                               
         B     CHKC090                                                          
*                                                                               
CHKC020C MVI   12(R1),C'C'                                                      
CHKC020D MVC   HALF,11(R3)                                                      
         CLI   HALF+1,C' '                                                      
         BH    *+14                                                             
         MVC   HALF+1(1),HALF                                                   
         MVI   HALF,C'0'                                                        
         PACK  DUB,HALF                                                         
         CVB   RF,DUB                                                           
         STC   RF,13(R1)                                                        
         B     CHKC090                                                          
*                                                                               
CHKC021  MVC   ADDRESS,0(R2)       GOTO DISPLAY                                 
         MVI   ACTION,4                                                         
         OI    FLAG1,FLRETQ                                                     
         BAS   RE,AREGOFF          BE SURE THIS IS OFF                          
         SAM24                                                                  
         B     XMOD1                                                            
*                                                                               
CHKC022  CLI   8(R3),C''''         A ' MEANS TEXT STRING                        
         BNE   CHKC023                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,5(R3)                                                         
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),9(R3)                                                    
         B     CHKC090                                                          
*                                                                               
CHKC023  BAS   RE,AREGON                                                        
         BAS   RE,CHKCODS                                                       
*                                                                               
CHKC090  LA    R3,24(R3)                                                        
         LA    R3,61(R3)                                                        
         BAS   RE,AREGOFF                                                       
         SAM24                                                                  
*                                                                               
         BCT   R0,CHKC001          22 TIMES DONE                                
         ST    R3,FULL1            SAVE CURRENT SCREEN POINTER                  
         B     CHKCXX                                                           
*                                                                               
CHKCXX   B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        CHECK CODE HEX INPUT                               *                   
*************************************************************                   
         SPACE 1                                                                
CHKCODS  NTR1                                                                   
         LA    RF,8(R3)                                                         
         LA    RE,8(R3)                                                         
         LA    R1,24(R3)                                                        
CHKCO1   MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
CHKCO2   LA    RE,1(RE)                                                         
         CR    RE,R1                                                            
         BNL   CHKCO5                                                           
         CLI   0(RE),C' '                                                       
         BH    CHKCO1                                                           
         B     CHKCO2                                                           
*                                                                               
CHKCO5   LA    R1,8(R3)                                                         
         SR    RF,R1                                                            
         GOTO1 HEXIN,DMCB,(0,8(R3)),(R2),(RF)                                   
*                                                                               
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        CHECK WATCH SCREEN                                 *                   
*************************************************************                   
         SPACE 1                                                                
CHKWATCH NTR1                                                                   
*                                                                               
         LA    R3,DEBTABH                                                       
*                                                                               
         LA    R1,WATCHES          SAC WATCH TABLE FOR AN ENTRY                 
         LA    RF,WATCHEX                                                       
CHKW010  OC    0(8,R1),0(R1)       IGNORE ZEROS                                 
         BNZ   CHKW030                                                          
*                                                                               
CHKW020  LA    R1,16(R1)           NEXT                                         
         CR    R1,RF                                                            
         BL    CHKW010                                                          
         B     CHKWATX             EOT                                          
*                                                                               
CHKW030  L     R3,FULL1            START OF WATCHES                             
         SR    R0,R0                                                            
CHKW040  ICM   R0,1,0(R3)                                                       
         BZ    CHKW020             ZERO = SCREEN DONE                           
*                                                                               
         MVC   DUB,8(R3)           DUB=FIELD                                    
         MVC   DUB2,0(R1)          DUB2=TABLE                                   
*                                                                               
         OC    DUB,SPACES                                                       
         OC    DUB2,SPACES                                                      
         CLC   DUB,DUB2            IS THIS MY WATCH FIELD                       
         BE    CHKW050                                                          
         AR    R3,R0               NEXT FIELD                                   
         B     CHKW040                                                          
*                                                                               
CHKW050  LR    RE,R3               POINT TO UNPROT DATA AREA                    
         AR    RE,R0                                                            
         CLI   8(RE),C'X'          DOES IT CONTAIN X                            
         BNE   CHKW020                                                          
*                                                                               
         XC    0(16,R1),0(R1)                                                   
         B     CHKW020                                                          
*                                                                               
CHKWATX  B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        CHECK HEX DISPLAY SCREEN                           *                   
*************************************************************                   
         SPACE 1                                                                
CHKHEXS  NTR1                                                                   
         LR    R3,R1                                                            
         CLC   INSCREEN,=X'FF01'                                                
         BNE   CHKHXX                                                           
         LH    R0,SIZE             MAX 22                                       
CHKH001  GOTO1 AHEXIN,DMCB,(0,8(R3)),FULL,8                                     
         L     R1,BASEADDR                                                      
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
*                                                                               
         SAM31                                                                  
         BAS   RE,AREGON                                                        
         L     R2,FULL             R2=ADDR                                      
         LA    R3,16(R3)                                                        
         LA    R4,4                THEN FOUR TIMES                              
*                                                                               
CHKH010  TM    4(R3),X'80'         ANY SEL INPUT                                
         BZ    *+8                                                              
         BAS   RE,CHKSEL                                                        
         LA    R3,9(R3)                                                         
         TM    4(R3),X'82'         ANY HEX INPUT                                
         BNO   *+8                                                              
         BAS   RE,CHKHEX                                                        
         LA    R3,16(R3)                                                        
         LA    R2,4(R2)                                                         
         BCT   R4,CHKH010          FOUR TIMES DONE                              
*                                                                               
         SH    R2,=H'16'                                                        
         TM    4(R3),X'80'         ANY CHR INPUT                                
         BZ    *+8                                                              
         BAS   RE,CHKCHR                                                        
         LA    R3,24(R3)                                                        
*                                                                               
         BAS   RE,AREGOFF                                                       
         SAM24                                                                  
*                                                                               
         BCT   R0,CHKH001          22 TIMES DONE                                
*                                                                               
CHKHXX   B     XITEQU                                                           
         EJECT                                                                  
************************************************************                    
*        DISP SCREEN HEX/CHR VALUES                        *                    
************************************************************                    
         SPACE 1                                                                
CHKHEX   ST    RE,SAVERE                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,(0,8(R3)),(R2),8                                      
*                                                                               
CHKHEXX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
CHKCHR   ST    RE,SAVERE                                                        
*                                                                               
         LA    RE,16                                                            
         LR    RF,R2                                                            
         LA    R1,8(R3)                                                         
CHKCHR1  CLI   0(R1),C'.'                                                       
         BE    *+10                                                             
         MVC   0(1,R2),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   RE,CHKCHR1                                                       
         LR    R2,RF                                                            
*                                                                               
CHKCHRX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
*        DISP SCREEN SEL VALUES                            *                    
************************************************************                    
         SPACE 1                                                                
CHKSEL   ST    RE,SAVERE                                                        
*                                                                               
         CLI   8(R3),C'Z'                                                       
         BE    CHKS050                                                          
         CLI   8(R3),C'G'                                                       
         BE    CHKS010                                                          
         CLI   8(R3),C'H'                                                       
         BE    CHKS010                                                          
         CLI   8(R3),C'?'                                                       
         BNE   CHKS020                                                          
*                                                                               
CHKS010  MVC   ADDRESS,0(R2)                                                    
         B     CHKSXX                                                           
*                                                                               
CHKS020  CLI   8(R3),C'%'                                                       
         BE    CHKS030                                                          
         CLI   8(R3),C'L'                                                       
         BNE   CHKS040                                                          
*                                                                               
CHKS030  MVC   ADDRESS,0(R2)                                                    
         MVI   ADDRESS,0                                                        
         B     CHKSXX                                                           
*                                                                               
CHKS040  CLI   8(R3),C'*'                                                       
         BNE   CHKS045                                                          
         ST    R2,ADDRESS                                                       
         B     CHKSXX                                                           
*                                                                               
CHKS045  CLI   8(R3),C'M'                                                       
         BNE   CHKSXX                                                           
         MVC   ADDRESS,0(R2)                                                    
         L     R1,SHMTADDR                                                      
         A     R1,ADDRESS                                                       
         ST    R1,ADDRESS                                                       
         B     CHKSXX                                                           
*                                                                               
CHKS050  LA    R1,200                                                           
CHKS051  CLC   0(5,R2),=C'DUMMY'                                                
         BNE   *+10                                                             
         XC    0(8,R2),0(R2)                                                    
         LA    R2,8(,R2)                                                        
         BCT   R1,CHKS051                                                       
*                                                                               
CHKSXX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
*        CHECK ADDRESS BOUNDARYS                           *                    
************************************************************                    
         SPACE 1                                                                
CHKADDR  ST    RE,SAVERE                                                        
*                                                                               
CHKADD1  L     R1,=A(ADDMAP-SAVED) USE SAVED ADDR MAP                           
         AR    R1,R7                                                            
*                                                                               
CHKADD2  CLC   ADDRESS,0(R1)       CHECK ADDRESS BOUNDS                         
         BH    *+10                                                             
         MVC   ADDRESS,0(R1)       IF < MINIMUM SET MINIMUM                     
*                                                                               
         CLC   ADDRESS,4(R1)       CHECK MAX                                    
         BL    CHKADD3                                                          
         LA    R1,8(R1)            SET NEXT BLOCK                               
         OC    0(4,R1),0(R1)                                                    
         BNZ   CHKADD2                                                          
         XC    ADDRESS,ADDRESS     ZAP ADDR TO ZERO IF WRAP AROUND              
         BZ    CHKADD1                                                          
*                                                                               
CHKADD3  ST    R1,ADRNDX           SET INDEX TO CURRENT BLOCK                   
*                                                                               
CHKADDRX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
*        BUILD CODE DISPLAY SCREEN                         *                    
************************************************************                    
         SPACE 1                                                                
BLDCODE  ST    RE,SAVERE           NO NTR1 (DESTROYS R0,R1,RE,RF)               
         OI    DEBMSGH+6,X'80'                                                  
         OI    DEBSRVH+6,X'80'                                                  
         OI    DEBAC1H+6,X'80'                                                  
         OI    DEBACTH+6,X'80'                                                  
         MVC   INSCREEN,=X'FF02'                                                
         SR    RE,RE                                                            
         LH    R0,SIZE             MAX 22                                       
         ICM   RE,3,2(R1)          SAVE SCREEN ADDR                             
*                                                                               
BLDCOD0  MVC   0(16,R1),HEXFLD1    MOVE IN 8 BYTES PROT                         
         STCM  RE,3,2(R1)                                                       
         LA    RE,9(RE)                                                         
         LA    R1,16(R1)                                                        
*                                                                               
BLDCO1   MVC   0(24,R1),CODFLD2    16 BYTE UNPROT                               
         STCM  RE,3,2(R1)                                                       
         LA    RE,17(RE)                                                        
         LA    R1,24(R1)                                                        
*                                                                               
BLDCO2   MVC   0(61,R1),CODFLD3    53 BYTE PROT                                 
         STCM  RE,3,2(R1)                                                       
         LA    RE,54(RE)                                                        
         LA    R1,61(R1)                                                        
*                                                                               
         BCT   R0,BLDCOD0          22 TIMES DONE                                
*                                                                               
         MVC   0(3,R1),=X'000101'  NEW END OF SCREEN                            
*                                                                               
BLDCODX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        FILL HEX DISPLAY SCREEN                            *                   
*************************************************************                   
         SPACE 1                                                                
FILLHEX  NTR1                                                                   
         SAM31                                                                  
*                                                                               
         LH    R4,SIZE             22 MAX                                       
         L     R2,ADDRESS                                                       
         LA    R3,DEBTABH                                                       
FILH010  SRL   R2,4                ALLIGN SCREEN                                
         SLL   R2,4                                                             
         LR    R1,R2                                                            
         S     R1,BASEADDR         IF BASEADDR REMOVE OFFSET                    
         ST    R1,FULL                                                          
         GOTOR HEXOUT,DMCB,FULL,8(R3),4                                         
         LA    R3,25(R3)                                                        
         LA    R0,4                                                             
*                                                                               
FILH011  EQU   *                                                                
         L     RE,ADRNDX                                                        
         CL    R2,4(RE)            CHECK BOUNDS HIGH                            
         BL    FILH012                                                          
*                                                                               
         MVC   8(8,R3),=C'........'                                             
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         MHI   RF,25                                                            
         AR    R3,RF                                                            
         LA    R3,16(,R3)          BUMP TO CHAR FIELD                           
         MVC   8(16,R3),SPACES     JUST FILL IN SPACES                          
*                                  (IGNORE CHARS FOR THIS LINE)                 
         LA    RE,8(RE)                                                         
         ICM   R2,15,0(RE)                                                      
         BNZ   *+10                                                             
         L     RE,=A(ADDMAP-SAVED)                                              
         AR    RE,R7                                                            
         ST    RE,ADRNDX                                                        
         B     FILH080                                                          
*                                                                               
FILH012  BAS   RE,AREGON                                                        
         GOTOR HEXOUT,DMCB,0(R2),8(R3),4                                        
         BAS   RE,AREGOFF                                                       
         LA    R3,25(R3)                                                        
         LA    R2,4(R2)                                                         
         BCT   R0,FILH011          NEXT FULL WORD                               
         SH    R3,=H'9'                                                         
         SH    R2,=H'16'                                                        
         BAS   RE,AREGON                                                        
         MVC   8(16,R3),0(R2)      CHARACTERS OUT                               
         BAS   RE,AREGOFF                                                       
         TR    8(16,R3),VALOCHRS                                                
         LA    R2,16(R2)                                                        
FILH080  LA    R3,24(R3)                                                        
         BCT   R4,FILH010          NEXT SCREEN LINE                             
         ST    R2,NEXTADD          SAVE FOR PF8                                 
*                                                                               
         LA    R3,DEBTABH+116      KILL OFF UP TO ALLIGNMENT                    
         L     R2,ADDRESS                                                       
         LR    R1,R2                                                            
         SRL   R1,4                                                             
         SLL   R1,4                                                             
         SR    R2,R1               R2 = NO OF BYTES TO KILL                     
*                                                                               
         LTR   R1,R2               FIRST THE CHRS                               
         BZ    FILH999                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),SPACES                                                   
         LA    R3,DEBTABH+25       THEN THE HEX                                 
*                                                                               
FILH090  LTR   R2,R2               REPLACE WITH ....S                           
         BZ    FILH999                                                          
         CH    R2,=H'4'                                                         
         BL    FILH091                                                          
         MVC   8(8,R3),SPACES                                                   
         LA    R3,25(R3)                                                        
         SH    R2,=H'4'            NEXT FULLWORD                                
         B     FILH090                                                          
FILH091  MVC   8(2,R3),SPACES                                                   
         LA    R3,2(R3)                                                         
         BCT   R2,FILH091          LAST FEW BYTES                               
*                                                                               
FILH999  SAM24                                                                  
*                                                                               
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        FILL DISS SCREEN                                   *                   
*************************************************************                   
         SPACE 1                                                                
FILLDIS  NTR1                                                                   
         XC    FULL1,FULL1                                                      
         ICM   R1,15,DBTCB                                                      
         BZ    *+16                                                             
         MVC   FULL1,TCBPSW+4-TCBD(R1)                                          
         NC    FULL1,=X'7FFFFFFF'                                               
         LH    R4,SIZE             22 LINES MAX                                 
         L     R2,ADDRESS                                                       
         LA    R3,DEBTABH                                                       
*                                                                               
         BAS   RE,AREGON                                                        
         SAM31                                                                  
*                                                                               
FILD010  ST    R2,FULL                                                          
         ICM   R1,15,BASEADDR      DON'T GO PAST BASE+LEN                       
         BZ    FILD011                                                          
         A     R1,BASELEN                                                       
         CR    R2,R1                                                            
         BNL   FILD990                                                          
*                                                                               
FILD011  L     R1,FULL1            TEST PSW-2                                   
         SHI   R1,2                                                             
         C     R1,FULL                                                          
         BNE   FILD015                                                          
         CLI   0(R2),0             MUST BE BREAK OF SOME SORT                   
         BNE   FILD015                                                          
*                                                                               
         NI    FLAG1,255-FLOCPSW                                                
         ST    R3,APSWMARK                                                      
         MVC   WORK1(8),=C'PSW -2 >'                                            
*                                                                               
FILD015  CLC   FULL,FULL1          PSW CHECK                                    
         BNE   FILD020                                                          
*                                                                               
         NI    FLAG1,255-FLOCPSW                                                
         ST    R3,APSWMARK                                                      
         MVC   WORK1(8),=C'PSW ===>'                                            
*                                                                               
FILD020  BAS   RE,AREGOFF                                                       
*                                                                               
         L     R1,FULL             SET UP OFFSET ADDR                           
         S     R1,BASEADDR                                                      
         ST    R1,OFFSET                                                        
*                                                                               
         GOTOR HEXOUT,DMCB,OFFSET,8(R3),4                                       
*                                                                               
*                                                                               
FILD030  BAS   RE,AREGON                                                        
         LA    R3,16(R3)                                                        
         GOTO1 =V(DEBDIS),DMCB,(R2),WORK,AHEXOUT,RR=RELO1                       
         SR    RF,RF                                                            
         IC    RF,4(R1)            GET ILC                                      
         BCTR  RF,0                                                             
         EX    RF,*+12                                                          
         EX    RF,*+14                                                          
         B     *+16                                                             
         MVC   WORK+50(0),0(R2)    CHARACTERS OUT                               
         TR    WORK+50(0),VALOCHRS                                              
         AR    R2,RF                                                            
         A     R2,=F'1'                                                         
*                                                                               
         MVC   8(16,R3),WORK                                                    
*                                                                               
         ICM   RF,15,AMYEXIT       SET A BREAKPOINT                             
         BZ    FILD040                                                          
         LA    RF,EXBREAK-EXAREAD(RF)                                           
         LA    R1,64                                                            
*                                                                               
FILD031  OC    0(4,RF),0(RF)       ZERO ENTRY                                   
         BZ    FILD040                                                          
         CLC   OFFSET,0(RF)        FOUND BREAKPOINT                             
         BE    FILD035                                                          
         LA    RF,4(RF)            TRY NEXT                                     
         BCT   R1,FILD031                                                       
         B     FILD040                                                          
*                                                                               
FILD035  OI    1(R3),X'08'         SET TO RED                                   
         MVC   8(4,R3),=C'BRK '                                                 
         BCT   R1,FILD031                                                       
*                                                                               
FILD040  LA    R3,24(R3)                                                        
         MVC   8(53,R3),WORK+16                                                 
         LA    R3,61(R3)                                                        
         BCT   R4,FILD010                                                       
*                                                                               
         ICM   R3,15,APSWMARK                                                   
         BZ    *+10                                                             
         MVC   8(8,R3),WORK1                                                    
         ST    R2,NEXTADD          SAVE FOR PF8                                 
*                                                                               
FILD990  BAS   RE,AREGOFF                                                       
         SAM31                                                                  
*                                                                               
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        FILL CODE SCREEN                                   *                   
*************************************************************                   
         SPACE 1                                                                
FILLCOD  NTR1                                                                   
         XC    FULL1,FULL1                                                      
         ICM   R1,15,DBTCB                                                      
         BZ    *+16                                                             
         MVC   FULL1,TCBPSW+4-TCBD(R1)                                          
         NC    FULL1,=X'7FFFFFFF'                                               
*                                                                               
         MVC   DUB+0(2),WRKFNO     OPEN MODULE                                  
         MVC   DUB+4(4),WRKFREC                                                 
         GOTO1 ARANDOM                                                          
*                                                                               
         LH    R4,SIZE             22 LINES MAX                                 
         L     R2,ADDRESS                                                       
         LA    R3,DEBTABH                                                       
         L     R5,AIO                                                           
*                                                                               
FILC010  EQU   *                                                                
*                                                                               
FILC012  XC    OFFSET,OFFSET       VALID HEX DISPLACEMENT                       
         GOTO1 AHEXIN,DMCB,5(R5),OFFSET+1,6                                     
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   FILC013                                                          
*                                                                               
         MVC   WORK(5),117(R5)     TEST VALID ASSEMBLER OUT                     
         NC    WORK(5),NUMBER                                                   
         CLC   WORK(5),NUMBER                                                   
         BNE   FILCNXT                                                          
         B     FILC015                                                          
*                                                                               
FILC013  L     R1,ADDRESS                                                       
         A     R1,OFFSET                                                        
         ST    R1,FULL                                                          
*                                                                               
         L     R1,FULL1            TEST PSW-2                                   
         SHI   R1,2                                                             
         C     R1,FULL                                                          
         BNE   FILC013A                                                         
*                                                                               
         NI    FLAG1,255-FLOCPSW                                                
         ST    R3,APSWMARK                                                      
         MVC   WORK1(8),=C'PSW -2 >'                                            
*                                                                               
FILC013A CLC   FULL,FULL1          PSW MUST BE SPOT ON FOR CODE                 
         BNE   FILC014                                                          
*                                                                               
         NI    FLAG1,255-FLOCPSW                                                
         ST    R3,APSWMARK                                                      
         MVC   WORK1(8),=C'PSW ===>'                                            
*                                                                               
FILC014  GOTOR HEXOUT,DMCB,OFFSET,8(R3),4                                       
*                                                                               
FILC015  LA    R3,16(R3)                                                        
*                                                                               
         TM    PAGEFLG,PAGERTQ     TEXT PART FIRST                              
         BO    *+14                                                             
         MVC   32(53,R3),41(R5)    LEFT HAND SIDE                               
         B     *+10                                                             
         MVC   32(53,R3),69(R5)    RIGHT HAND SIDE                              
*                                                                               
         CLI   BASETY,C'D'         DSECT OR CSECT                               
         BE    *+12                                                             
         BAS   RE,CSFILL           CSECTS                                       
         B     FILC020                                                          
         BAS   RE,DSFILL           DSECTS                                       
*                                                                               
FILC020  LA    R3,24(R3)                                                        
         LA    R3,61(R3)                                                        
*                                                                               
FILC030  BCT   R4,FILCNXT                                                       
         ICM   R3,15,APSWMARK                                                   
         BZ    XITEQU                                                           
         OI    1(R3),X'08'                                                      
*                                                                               
         LA    R1,16(R3)           NEXT FIELD                                   
         CLC   8(5,R1),=C'BREAK'                                                
         BE    *+14                                                             
         MVC   8(8,R3),WORK1                                                    
         B     XITEQU                                                           
*                                                                               
         MVC   8(12,R1),=C'BREAK <==PSW'                                        
         B     XITEQU                                                           
*                                                                               
FILCNXT  GOTO1 AREADMOD            READ NEXT LINE                               
         BNE   FILC050                                                          
         B     FILC010                                                          
*                                                                               
FILC050  OI    PAGEFLG,PAGENDQ     FLAG LAST PAGE                               
         SH    R3,=H'61'                                                        
         MVC   8(53,R3),SPACES                                                  
         MVC   8(30,R3),=C'**** END OF DSECT / CSECT ****'                      
         OI    1(R3),X'08'                                                      
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        CSECT FILLER R5=WRKF LINE R3=FIELD FULL=ADDR       *                   
*************************************************************                   
         SPACE 1                                                                
CSFILL   NTR1                                                                   
         ICM   R2,15,FULL          ADDR MUST BE IN FULL                         
         BZ    CSFILLX                                                          
*                                                                               
         BAS   RE,AREGON                                                        
         SAM31                                                                  
*                                                                               
         CLI   16(R5),C' '         TEST FOR HEXIN                               
         BNE   CSF050                                                           
*                                                                               
         LA    R4,12(R5)                                                        
         MVI   BYTE,0                                                           
         LA    R0,3                                                             
CSF010   MVC   WORK(4),0(R4)                                                    
         CLI   WORK,C' '                                                        
         BE    CSF020                                                           
         GOTO1 AHEXIN,DMCB,WORK,HALF,4                                          
         CLC   HALF,0(R2)                                                       
         BE    CSFILLCN                                                         
*                                                                               
         CLI   BYTE,1              AFTER FIRST 0002 JUST TEST BYTE              
         BE    *+14                                                             
         CLC   0(2,R2),=X'0002'    SPECIAL CODE FOR REPLACE                     
         BNE   CSFILLCO                                                         
         MVC   0(2,R2),HALF        COPY WRKF BOOK TO CORE                       
         MVI   BYTE,1              FLAG THIS FOR THE REST                       
*                                                                               
CSFILLCN ICM   RF,15,AMYEXIT       SET A BREAKPOINT                             
         BZ    CSF015                                                           
         LA    RF,EXBREAK-EXAREAD(RF)                                           
         LA    R1,64                                                            
*                                                                               
CSFILLB  OC    0(4,RF),0(RF)       ZERO ENTRY                                   
         BZ    CSF015                                                           
         C     R2,0(RF)            FOUND BREAKPOINT                             
         BE    CSFILLB1                                                         
         LA    RF,4(RF)            TRY NEXT                                     
         BCT   R1,CSFILLB                                                       
         B     CSF015                                                           
*                                                                               
CSFILLB1 OI    1(R3),X'08'         SET TO RED                                   
         MVC   8(5,R3),=C'BREAK'                                                
         B     CSFILLNX                                                         
*                                                                               
CSF015   LA    R2,2(R2)                                                         
         LA    R4,5(R4)                                                         
         BCT   R0,CSF010                                                        
CSF020   MVC   8(15,R3),12(R5)                                                  
         B     CSFILLX                                                          
*                                                                               
CSF050   LA    R1,12(R5)                                                        
         LR    R4,R1                                                            
CSF051   CLI   0(R4),C' '                                                       
         BE    CSF052                                                           
         LA    R4,1(R4)                                                         
         B     CSF051                                                           
*                                                                               
CSF052   SR    R4,R1                                                            
         GOTO1 AHEXIN,DMCB,(0,12(R5)),WORK,(R4)                                 
         SRL   R4,1                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),WORK                                                     
         BE    CSF053                                                           
         CLC   0(2,R2),=X'0002'                                                 
         BNE   CSFILLN                                                          
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK                                                     
*                                                                               
CSF053   MVC   8(16,R3),12(R5)                                                  
*                                                                               
         B     CSFILLX                                                          
*                                                                               
CSFILLN  OI    1(R3),X'08'         SET TO RED                                   
         LA    R4,1(R4)                                                         
         L     R1,FULL                                                          
         ST    R1,DMCB                                                          
         GOTO1 AHEXOUT,DMCB,,(0,8(R3)),(R4)                                     
         B     CSFILLNX                                                         
*                                                                               
CSFILLCO L     R1,FULL                                                          
         ST    R1,DMCB                                                          
         GOTO1 =V(DEBDIS),DMCB,,WORK,AHEXOUT,RR=RELO1                           
         OI    1(R3),X'08'         SET TO WHITE                                 
         MVC   8(16,R3),WORK                                                    
         LA    R3,24(R3)                                                        
         OI    1(R3),X'08'                                                      
         MVC   20(20,R3),WORK+16                                                
*                                                                               
CSFILLNX CLI   22(R3),C' '                                                      
         BNE   *+8                                                              
         MVI   23(R3),C' '                                                      
*                                                                               
CSFILLX  BAS   RE,AREGOFF                                                       
         SAM24                                                                  
*                                                                               
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        DSECT FILLER R5=WRKF LINE R3=FIELD FULL=ADDR       *                   
*************************************************************                   
         SPACE 1                                                                
DSFILL   NTR1                                                                   
         ICM   R2,15,FULL          ADDR MUST BE IN FULL                         
         BZ    DSFILLX                                                          
*                                                                               
         CLC   54(2,R5),=C'DS'     THIS ROUTINE IS FOR DS                       
         BE    *+14                                                             
         CLC   54(2,R5),=C'DC'     OR DC                                        
         BNE   DSFILLX                                                          
*                                                                               
DSF010   MVC   MLEN,=F'1'                                                       
         MVC   LLEN,=F'1'                                                       
         LA    R1,60(R5)                                                        
         GOTO1 AVALNUM                                                          
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         ST    R1,MLEN                                                          
*                                                                               
DSF030   L     R1,FULL                                                          
         LA    RF,TYPTAB                                                        
DSF031   CLC   0(1,R1),0(RF)                                                    
         BE    DSF040                                                           
         LA    RF,2(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   DSF031                                                           
         B     DSFILLX                                                          
*                                                                               
DSF040   MVC   BYTE,0(RF)                                                       
         MVC   LLEN+3(1),1(RF)                                                  
*                                                                               
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'L'                                                       
         BNE   DSF045                                                           
         LA    R1,1(R1)                                                         
         GOTO1 AVALNUM                                                          
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         ST    R1,LLEN                                                          
*                                                                               
DSF045   SAM31                                                                  
         BAS   RE,AREGON                                                        
         L     R1,MLEN                                                          
         MH    R1,LLEN+2                                                        
         CLI   BYTE,C'C'                                                        
         BNE   DSF060                                                           
         CLI   0(R2),C' '          TEST AT LEAST ONE CHR                        
         BL    DSF060                                                           
*                                                                               
DSF050   CH    R1,=H'16'                                                        
         BL    *+8                                                              
         LH    R1,=H'16'                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),0(R2)                                                    
         B     DSFILLX                                                          
*                                                                               
DSF060   CH    R1,=H'8'                                                         
         BL    *+8                                                              
         LH    R1,=H'8'                                                         
         LR    RF,R1                                                            
         GOTOR HEXOUT,DMCB,(R2),(0,8(R3)),(RF)                                  
         B     DSFILLX                                                          
*                                                                               
DSFILLX  BAS   RE,AREGOFF                                                       
         SAM24                                                                  
         B     XITEQU                                                           
         SPACE 1                                                                
TYPTAB   DC    C'A',X'04'                                                       
         DC    C'F',X'04'                                                       
         DC    C'V',X'04'                                                       
         DC    C'H',X'02'                                                       
         DC    C'C',X'01'                                                       
         DC    C'X',X'01'                                                       
         DC    X'FFFF'                                                          
         EJECT                                                                  
*************************************************************                   
*        SET SCREEN SIZE                                    *                   
*************************************************************                   
         SPACE 1                                                                
SETSIZE  BR    RE                  IGNORE                                       
         MVC   SIZE,=H'10'                                                      
         MVC   SCROLL,=H'10'                                                    
SETSIZEX B     XITEQU                                                           
         EJECT                                                                  
*OLD BLDWATCH LOCATION                                                          
         EJECT                                                                  
*************************************************************                   
*        FIND STRING                                        *                   
*************************************************************                   
         SPACE 1                                                                
FINDSTR  NTR1                                                                   
         SAM31                     SET 31 BIT + AR MODE IF SET                  
         BAS   RE,AREGON                                                        
         L     R0,=X'000FFFFF'     1024K SEARCH                                 
         L     R2,ADDRESS                                                       
         SR    RF,RF                                                            
         ICM   RF,1,INFLEN         EX COMPARE FOR STRING                        
         BAS   RE,WHATWAY          ADD OR SUB 1                                 
         SR    RF,RF                                                            
         ICM   RF,1,INFLEN         EX COMPARE FOR STRING                        
         BNZ   FIND005                                                          
         MVC   INFLEN(61),PREVFLD  TRY PREVIOUS IF NOT SET                      
         ICM   RF,1,INFLEN                                                      
         BZ    FINDNO                                                           
*                                                                               
FIND005  MVC   PREVFLD,INFLEN      SAVE THIS ON FIND ACTION                     
         BCTR  RF,0                                                             
FIND010  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),INFIELD                                                  
         BE    FINDOK                                                           
         BAS   RE,WHATWAY          ADD OR SUB 1 IF NOT FOUND                    
         BCT   R0,FIND010                                                       
         B     FINDNO                                                           
*                                                                               
FINDOK   ST    R2,ADDRESS          SET ADDR WHEN FOUND                          
         SAM24                                                                  
         BAS   RE,AREGOFF                                                       
         MVC   INFO,=H'134'        STRING FOUND                                 
         B     XITEQU                                                           
*                                                                               
FINDNO   ST    R2,ADDRESS          SET ADDR                                     
         SAM24                                                                  
         BAS   RE,AREGOFF                                                       
         MVC   INFO,=H'135'        NOT FOUND                                    
         B     XITNEQ              SET NEQ IF NOT FOUND                         
*                                                                               
WHATWAY  CLI   PFKEY,7             PF 7 SEARCH BACK                             
         BE    WHATWAY1                                                         
         LA    R2,1(R2)            ELSE SEARCH FORWARD                          
         L     R1,ADRNDX                                                        
         LA    R2,1(RF,R2)                                                      
         CL    R2,4(R1)            CHECK BOUNDS HIGH                            
         BNL   *+10                                                             
         SR    R2,RF                                                            
         BCTR  R2,0                                                             
         BR    RE                                                               
         LA    R1,8(R1)            BUMP TO NEXT                                 
         ICM   R2,15,0(R1)                                                      
         BZ    XITNEQ              NOT FOUNDS IF ZERO                           
         ST    R1,ADRNDX           NEW INDEX                                    
         BR    RE                                                               
*                                                                               
WHATWAY1 BCTR  R2,0                                                             
         L     R1,ADRNDX                                                        
         CL    R2,0(R1)            CHECK BOUNDS LOW                             
         BHR   RE                                                               
         LTR   R2,R2                                                            
         BZ    XITNEQ                                                           
         C     R2,LOCORE                                                        
         BE    XITNEQ                                                           
         SH    R1,=H'8'            BUMP TO PREV                                 
         ICM   R2,15,4(R1)                                                      
         BZ    XITNEQ              NOT FOUNDS IF ZERO                           
         SR    R2,RF                                                            
         ST    R1,ADRNDX           NEW INDEX                                    
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        FIND STRING IN WORKER FILE                         *                   
*************************************************************                   
         SPACE 1                                                                
FINDWRK  NTR1                                                                   
         MVC   DUB+0(2),WRKFNO     OPEN MODULE                                  
         MVC   DUB+4(4),WRKFREC                                                 
         GOTO1 ARANDOM                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,INFLEN         EX COMPARE FOR STRING                        
         BNZ   FINDW020                                                         
         MVC   INFLEN(61),PREVFLD  TRY PREVIOUS IF NOT SET                      
         B     FINDW020                                                         
*                                                                               
FINDW002 ICM   RF,1,INFLEN                                                      
         BZ    FINDWNO                                                          
         L     R5,AIO                                                           
         LA    R5,45(R5)                                                        
         LA    R0,80                                                            
*                                                                               
FINDW005 MVC   PREVFLD,INFLEN      SAVE THIS ON FIND ACTION                     
         BCTR  RF,0                                                             
FINDW010 EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),INFIELD                                                  
         BE    FINDWOK                                                          
         LA    R5,1(R5)            ADD OR SUB 1 IF NOT FOUND                    
         BCT   R0,FINDW010                                                      
*                                                                               
FINDW020 L     R1,WRKFREC                                                       
         LA    R1,1(R1)                                                         
         ST    R1,WRKFREC                                                       
         GOTO1 AREADMOD            READ NEXT RECORD                             
         BNE   FINDWNO                                                          
         B     FINDW002                                                         
*                                                                               
FINDWOK  MVC   INFO,=H'134'        STRING FOUND                                 
         B     XITEQU                                                           
*                                                                               
FINDWNO  MVC   INFO,=H'135'        STRING NOT FOUND                             
         B     XITNEQ              SET NEQ IF NOT FOUND                         
         EJECT                                                                  
*************************************************************                   
*        GET WRKF DETAIL FOR LIST                           *                   
*************************************************************                   
         SPACE 1                                                                
GETWRKF  NTR1                                                                   
         TM    FLAG1,FLNEWQ        ONLY GET ON NEW ACTION                       
         BZ    GETWKX                                                           
         MVI   TWAPAGE,2                                                        
         GOTO1 AREADTWA            GET PAGE 2 MODULE LIST                       
         L     R1,ATIA                                                          
         CLC   0(8,R1),=C'**MODS**'                                             
         BNE   XITNEQ                                                           
         LA    R1,8(R1)                                                         
*                                                                               
GETWK010 CLC   0(8,R1),=C'**ENDM**'                                             
         BE    XITNEQ                                                           
         CLC   INFIELD(8),0(R1)                                                 
         BE    GETWK020                                                         
         LA    R1,20(R1)                                                        
         B     GETWK010                                                         
*                                                                               
GETWK020 MVC   WRKFNO,8(R1)                                                     
         MVC   WRKFBASE,10(R1)                                                  
         MVC   WRKFREC,10(R1)                                                   
         OC    16(4,R1),16(R1)                                                  
         BZ    *+10                                                             
         MVC   ADDRESS,16(R1)                                                   
         MVC   BASETY,14(R1)                                                    
*                                                                               
GETWKX   B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        PF KEY HANDLING                                    *                   
*************************************************************                   
         SPACE 1                                                                
CHECKPF  ST    RE,SAVERE                                                        
         MVC   FULL,ADDRESS        SAVE CURRENT IN FULL                         
         CLI   PFKEY,0             ADJUST FOR PFKEYS                            
         BE    CHECKPFX                                                         
*                                                                               
         CLI   PFKEY,10            LEFT AND RIGHT                               
         BNE   *+8                                                              
         NI    PAGEFLG,255-PAGERTQ                                              
         CLI   PFKEY,11                                                         
         BNE   *+8                                                              
         OI    PAGEFLG,PAGERTQ                                                  
*                                                                               
         CLI   SUBACT,3            TAB SUB ACT                                  
         BE    CHKP010                                                          
*                                                                               
CHKP001  CLI   ACTION,6            LIST ACTION                                  
         BNE   CHKP010                                                          
*                                                                               
         ICM   R1,15,WRKFREC                                                    
         CLI   PFKEY,7             7 = UP                                       
         BNE   CHKP005                                                          
         OC    DECFLD,DECFLD                                                    
         BZ    *+12                                                             
         S     R1,DECFLD                                                        
         B     *+8                                                              
         SH    R1,SCROLL           SCROLL DEF=22                                
         C     R1,WRKFBASE                                                      
         BH    *+8                                                              
         L     R1,WRKFBASE                                                      
         B     CHKP006                                                          
CHKP005  CLI   PFKEY,8             8 = DOWN                                     
         BNE   CHECKPFX                                                         
         TM    PAGEFLG,PAGENDQ     DOWN NOT POSSIBLE ON LAST PAGE               
         BO    CHECKPFX                                                         
         OC    DECFLD,DECFLD                                                    
         BZ    *+12                                                             
         A     R1,DECFLD                                                        
         B     *+8                                                              
         AH    R1,SCROLL                                                        
CHKP006  STCM  R1,15,WRKFREC                                                    
         B     CHECKPFX                                                         
*                                                                               
CHKP010  L     R1,ADDRESS                                                       
         CLI   PFKEY,7             7 = UP                                       
         BNE   CHKP050                                                          
         OC    HEXFLD,HEXFLD       ANY HEX INPUT                                
         BNZ   CHKP011                                                          
         OC    DECFLD,DECFLD       ANY DEC INPUT                                
         BZ    CHKP015                                                          
*                                                                               
CHKP011  L     RF,DECFLD           BACK UP BY DEC INPUT                         
         CLI   SUBACT,3            IF TAB FUNCTION                              
         BE    *+8                                                              
         SLL   RF,4                OR 16 TIMES HEX INPUT IF NOT                 
         SR    R1,RF                                                            
         ST    R1,ADDRESS                                                       
         B     CHECKPFX                                                         
*                                                                               
CHKP015  L     RF,NEXTADD          SUBTRACT CURRENT SCREEN SIZE                 
         SR    RF,R1                                                            
         SR    R1,RF                                                            
         ST    R1,ADDRESS          THIS IS NOT GREAT FOR DISS!                  
         B     CHECKPFX                                                         
*                                                                               
CHKP050  CLI   PFKEY,8             8 = DOWN                                     
         BNE   CHECKPFX                                                         
         OC    HEXFLD,HEXFLD       ANY HEX INPUT                                
         BNZ   CHKP051                                                          
         OC    DECFLD,DECFLD       ANY DEC INPUT                                
         BZ    CHKP055                                                          
*                                                                               
CHKP051  L     RF,DECFLD           FORWARD BY DEC INPUT                         
         CLI   SUBACT,3            IF TAB FUNCTION                              
         BE    *+8                                                              
         SLL   RF,4                OR 16 TIMES HEX INPUT IF NOT                 
         AR    R1,RF                                                            
         ST    R1,ADDRESS                                                       
         B     CHECKPFX                                                         
*                                                                               
CHKP055  MVC   ADDRESS,NEXTADD     SET ADDRESS FROM NEXTADD                     
*                                                                               
CHECKPFX ICM   R1,15,BASEADDR      ANY BASE ADDRESS                             
         BZR   RE                                                               
         A     R1,BASELEN          DON'T GO PAST BASE LEN                       
         C     R1,ADDRESS                                                       
         BH    *+10                                                             
         MVC   ADDRESS,FULL        RESTORE OLD ADDRESS                          
*                                                                               
         CLC   ADDRESS,BASEADDR    DON'T ALLOW LESS THAN BASEADDR               
         BNLR  RE                                                               
         MVC   ADDRESS,BASEADDR                                                 
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
*        HEXIN PARMS = SOUCE/DEST/LEN                      *                    
************************************************************                    
         SPACE 1                                                                
HEXIN    NTR1                                                                   
*                                                                               
         L     R3,0(R1)                                                         
         L     R2,4(R1)                                                         
         L     R4,8(R1)                                                         
         LTR   R4,R4                                                            
         BNP   HEXIERR                       SOURCE LENGTH NOT POSITIVE         
         MVI   BYTE,0                                                           
*                                                                               
*        TRANSLATE SOURCE CHARACTERS A LE NOAH DE L'ARC                         
*                                                                               
         SR    R7,R7                                                            
         LR    R5,R4                                                            
HEXI1    SR    R6,R6                         SET TO FIRST OF PAIR               
HEXI2    IC    R7,0(R3,R6)                                                      
         CH    R7,=H'240'                                                       
         BL    HEXI3                         SOURCE CHR LT C'0'                 
         CH    R7,=H'249'                                                       
         BH    HEXI6                         SOURCE CHR GT C'9'                 
         STC   R7,HALF(R6)                                                      
         B     HEXI4                                                            
HEXI3    CH    R7,=H'193'                                                       
         BL    HEXI6                         SOURCE CHR LT C'A'                 
         CH    R7,=H'198'                                                       
         BH    HEXI6                         SOURCE CHR GT C'F'                 
         N     R7,=F'15'                     CHR FROM X'CN' TO X'0N'            
         AH    R7,=H'09'                     C'A'=X'01' TO X'0A'                
         STC   R7,HALF(R6)                                                      
HEXI4    LTR   R6,R6                                                            
         BP    HEXI5                                                            
         CH    R5,=H'1'                                                         
         BNE   *+20                                                             
         LA    R5,1(R5)                      LAST ODD SOURCE CHR                
         LA    R7,240                        SIMULATE C'0'                      
         LA    R6,1                                                             
         B     HEXI2+4                                                          
         LA    R6,1                          SET TO SECOND OF PAIR              
         B     HEXI2                                                            
HEXI5    UNPK  HALF+1(1),HALF+1(1)           HALF=X'.N.M' TO X'.NM.'            
         LH    R6,HALF                                                          
         SRL   R6,4                          R6=X'.NM.' TO X'..NM'              
         STC   R6,0(,R2)                                                        
         LA    R3,2(R3)                      UP SOURCE PTR BY 2                 
         LA    R2,1(R2)                      UP DESTN  PTR BY 1                 
         SH    R5,=H'1'                                                         
         BCT   R5,HEXI1                      DOWN SOURCE COUNTER BY 2           
*                                                                               
         CLI   BYTE,1                                                           
         BE    HEXIERR                                                          
         LA    R5,1(R4)                                                         
         SRA   R5,1                          RETURN DESTN LENGTH                
         B     HEXIX                                                            
*                                                                               
HEXI6    MVI   BYTE,1                        SET ERROR FLAG                     
         LH    R7,=H'240'                    REPLACE BY C'0'                    
         B     HEXI2+4                                                          
HEXIERR  SR    R5,R5                         RETURN ERROR VALUE OF ZERO         
*                                                                               
HEXIX    ST    R5,12(R1)                                                        
         XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        ERROR EXITS                                        *                   
*************************************************************                   
         SPACE 1                                                                
ERR1     MVC   ERROR,=H'11'                                                     
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LITERALS                               *                   
*************************************************************                   
         SPACE 1                                                                
DMREAD   DC    CL8'DMREAD'                                                      
TEMPSTR  DC    CL8'TEMPSTR'                                                     
WRKFILE  DC    CL8'WRKFILE'                                                     
INDEX    DC    CL8'INDEX  '                                                     
RANDOM   DC    CL8'RANDOM '                                                     
READ     DC    CL8'READ   '                                                     
NUMBER   DC    CL8'00000000'                                                    
ZEROS    DC    4F'0'                                                            
*                                                                               
HEXFLD1  DC    X'1020000000008000',C'        '                                  
HEXFLD2  DC    X'0908000000008000',C' '                                         
HEXFLD3  DC    X'1820000000008000',C'................'                          
CODFLD2  DC    X'1800000000008000'                                              
         DC    16C' '                                                           
CODFLD3  DC    X'3D20000000008000'                                              
         DC    53C' '                                                           
SPACES   DC    80C' '                                                           
*                                                                               
         EJECT                                                                  
VALOCHRS DC    CL32'................................'  00-1F                    
         DC    CL32'................................'  20-3F                    
         DC    CL32' ..........<(+|&&.........!$*);.' 40-5F                    
         DC    CL32'-/........º,%.>?.........`:#@''="' 60-7F                    
         DC    XL16'4B8182838485868788894B4B4B4B4B4B'  80-8F                    
         DC    XL16'4B9192939495969798994B4B4B4B4B4B'  90-9F                    
         DC    XL16'4BA1A2A3A4A5A6A7A8A94B4B4B4B4B4B'  A0-AF                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  B0-BF                    
         DC    CL32'{ABCDEFGHI......}JKLMNOPQR......'  C0-DF                    
         DC    CL32'\.STUVWXYZ......0123456789......'  E0-FF                    
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
         DROP  RB,RA                                                            
************************************************************                    
*        HEXOUT PARMS = SOUCE/DEST/LEN                     *                    
************************************************************                    
         SPACE 1                                                                
HEXOUT   NTR1  BASE=*                                                           
*                                                                               
         LM    R2,R4,0(R1)                                                      
         LTR   R4,R4                                                            
         BP    HEXO1                                                            
         SR    R6,R6                         RETURN ZERO LENGTH LE ZERO         
         B     HEXOX                                                            
*                                                                               
*        CONVERT EACH SOURCE CHR X'ZD' TO TWO DESTN CHRS X'0Z' & X'0D'          
*                                                                               
HEXO1    LA    R6,1(R3)                                                         
HEXO3    UNPK  0(1,R3),0(1,R2)                                                  
         NI    0(R3),X'0F'                                                      
         MVN   0(1,R6),0(R2)                                                    
         NI    0(R6),X'0F'                                                      
         LA    R2,1(R2)                      UP SOURCE PTR                      
         LA    R3,2(R3)                      UP DESTN  ZONE PTR                 
         LA    R6,2(R6)                      UP DESTN  DIGT PTR                 
         BCT   R4,HEXO3                                                         
*                                                                               
         LM    R3,R4,4(R1)                   R3=A(DESTN)                        
         SLA   R4,1                          R4=L'DESTN                         
         LR    R6,R4                                                            
HEXO4    CH    R4,=H'256'                    TRANSLATE DESTN                    
         BL    HEXO6                                                            
         TR    0(256,R3),=C'0123456789ABCDEF'                                   
         SH    R4,=H'256'                                                       
         LA    R3,256(R3)                                                       
         B     HEXO4                                                            
HEXO5    TR    0(0,R3),=C'0123456789ABCDEF'                                     
HEXO6    LTR   R4,R4                                                            
         BZ    HEXOX                                                            
         BCTR  R4,0                                                             
         EX    R4,HEXO5                                                         
*                                                                               
HEXOX    ST    R6,16(R1)                                                        
         XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DISPLAY WATCHES                                    *                   
*************************************************************                   
         SPACE 1                                                                
BLDWATCH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R5,R5               USE R5 TO KEEP SCREEN ADDRESS                
*                                                                               
         LA    R3,DEBTABH          DEBTAB IS AT LINE 3                          
DWAT010  SR    R0,R0                                                            
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         CLC   0(3,R3),=X'000101'  FIND END OF SCREEN                           
         BNE   DWAT010                                                          
         LA    RF,22                                                            
         SH    RF,SIZE             RF=NUMBER OF WATCH LINES                     
*                                                                               
         LH    R5,SIZE             COMPUTE START POSITION                       
         LA    R5,2(R5)                                                         
         MH    R5,=H'80'           80 * (SIZE+2)                                
         LA    R5,1(R5)            +1                                           
*                                                                               
         CLI   WATCHFLG,0          ANY DEFAULT WATCH LINES                      
         BZ    DWAT019                                                          
         EJECT                                                                  
*************************************************************                   
*        BUILD PSW WATCH                                    *                   
*************************************************************                   
         SPACE 1                                                                
         TM    WATCHFLG,X'80'      PSW WATCH                                    
         BZ    DWAT011                                                          
*                                                                               
         MVC   0(15,R3),WATFLD1    PSW HEADER                                   
         STCM  R5,3,2(R3)                                                       
         MVC   8(7,R3),=C'PSW =   '                                             
         LA    R3,15(R3)                                                        
*                                                                               
         LA    R5,8(R5)            NEXT FIELD                                   
         MVC   0(78,R3),WATFLD2    DATA FIELD                                   
         STCM  R5,3,2(R3)                                                       
         LA    R5,72(R5)           NEXT LINE                                    
*                                                                               
         L     R2,DBTCB                                                         
         LA    R2,TCBPSW-TCBD(R2)                                               
         GOTO1 AHEXOUT,DMCB,(R2),WORK,4 HEXOUT PSW                              
         MVC   8(8,R3),WORK                                                     
         LA    R2,4(R2)                                                         
         GOTO1 AHEXOUT,DMCB,(R2),WORK,4                                         
         MVC   17(8,R3),WORK                                                    
*                                                                               
         L     R2,DBTCB                                                         
         MVC   DUB+0(4),TCBPSW+4-TCBD(R2)                                       
         MVC   DUB+4(4),TCBPSW+52-TCBD(R2)                                      
         L     R1,DUB+0                                                         
         S     R1,DUB+4                                                         
         ST    R1,FULL                     CALC PSW-RB                          
*                                                                               
         MVC   26(33,R3),=C'YOU STOPPED IN ******** AT 000000'                  
         GOTO1 AHEXOUT,DMCB,FULL+1,53(R3),3                                     
         L     R1,DUB+4                                                         
         MVC   41(8,R3),22(R1)                                                  
*                                                                               
         LA    R3,78(R3)                                                        
         EJECT                                                                  
*************************************************************                   
*        BUILD REGISTER WATCH                               *                   
*************************************************************                   
         SPACE 1                                                                
DWAT011  TM    WATCHFLG,X'40'      REGS WATCH                                   
         BZ    DWAT012                                                          
*                                                                               
         MVC   0(15,R3),WATFLD1                                                 
         STCM  R5,3,2(R3)                                                       
         MVC   8(7,R3),=C'GR 0-7 '                                              
         LA    R3,15(R3)                                                        
         LA    R5,8(R5)            NEXT FIELD                                   
         MVC   0(78,R3),WATFLD2    DATA FIELD                                   
         MVI   0(R3),79            71 FOR REGS                                  
         STCM  R5,3,2(R3)                                                       
         LA    R5,72(R5)                                                        
*                                                                               
         LA    R0,8                                                             
         L     R2,DBTCB                                                         
         LA    R2,TCBPSW-TCBD(R2)                                               
         LA    R2,8(R2)                                                         
         LA    R4,WORK                                                          
         MVC   WORK(80),=CL80' '                                                
DWAT011A GOTO1 AHEXOUT,DMCB,(R2),(R4),4                                         
         LA    R2,4(R2)                                                         
         LA    R4,9(R4)                                                         
         BCT   R0,DWAT011A                                                      
         MVC   8(71,R3),WORK                                                    
         LA    R3,79(R3)                                                        
*                                                                               
         MVC   0(15,R3),WATFLD1                                                 
         STCM  R5,3,2(R3)                                                       
         MVC   8(7,R3),=C'GR 8-F '                                              
         LA    R3,15(R3)                                                        
         LA    R5,8(R5)            NEXT FIELD                                   
         MVC   0(78,R3),WATFLD2    DATA FIELD                                   
         MVI   0(R3),79            71 FOR REGS                                  
         STCM  R5,3,2(R3)                                                       
         LA    R5,72(R5)                                                        
*                                                                               
         LA    R0,8                                                             
         L     R2,DBTCB                                                         
         LA    R2,TCBPSW-TCBD(R2)                                               
         LA    R2,40(R2)                                                        
         LA    R4,WORK                                                          
         MVC   WORK(80),=CL80' '                                                
DWAT011B GOTO1 AHEXOUT,DMCB,(R2),(R4),4                                         
         LA    R2,4(R2)                                                         
         LA    R4,9(R4)                                                         
         BCT   R0,DWAT011B                                                      
         MVC   8(71,R3),WORK                                                    
         LA    R3,79(R3)                                                        
         MVC   WORK(80),=CL80' '                                                
         EJECT                                                                  
*************************************************************                   
*        BUILD OPERAND WATCH                                *                   
*************************************************************                   
         SPACE 1                                                                
DWAT012  TM    WATCHFLG,X'20'      OPERAND WATCH                                
         BZ    DWAT015                                                          
*                                                                               
         LR    R4,R3               SAVE A(OPER LINE)                            
         MVC   0(15,R3),WATFLD1                                                 
         STCM  R5,3,2(R3)                                                       
         MVC   8(7,R3),=C'OPER 1 '                                              
         LA    R3,15(R3)                                                        
         LA    R5,8(R5)            NEXT FIELD                                   
         MVC   0(39,R3),WATFLD2    DATA FIELD                                   
         MVI   0(R3),39            31 FOR OPER                                  
         STCM  R5,3,2(R3)                                                       
         LA    R5,32(R5)                                                        
         LA    R3,39(R3)                                                        
*                                                                               
         MVC   0(15,R3),WATFLD1                                                 
         STCM  R5,3,2(R3)                                                       
         MVC   8(7,R3),=C'OPER 2 '                                              
         LA    R3,15(R3)                                                        
         LA    R5,8(R5)            NEXT FIELD                                   
         MVC   0(39,R3),WATFLD2    DATA FIELD                                   
         MVI   0(R3),39            31 FOR OPER                                  
         STCM  R5,3,2(R3)                                                       
         LA    R5,32(R5)                                                        
         LA    R3,39(R3)                                                        
*                                                                               
         L     R2,DBTCB                                                         
         LA    R2,TCBPSW-TCBD(R2)                                               
         L     R2,4(R2)            POINT TO INSTRUCTION                         
         GOTO1 =V(DEBDIS),DMCB,(R2),WORK,AHEXOUT,RR=RELO1                       
         SR    RF,RF                                                            
         IC    RF,4(R1)            GET ILC                                      
         MVC   BYTE,5(R1)          SAVE QTYPE                                   
         CHI   RF,2                                                             
         BE    DWAT015             ILC 2 FORGET IT                              
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,2(R2)          R1=0000BDDD                                  
*                                                                               
         XC    HALF,HALF                                                        
         CHI   RF,4                                                             
         BE    *+10                ILC 4 NO MORE                                
         MVC   HALF,4(R2)                                                       
*                                                                               
         XR    RF,RF                                                            
         SRL   R1,12               0000000B                                     
         SLL   R1,2                BASE*4                                       
         LTR   R1,R1                                                            
         BZ    DWAT012A            BASE = 0                                     
*                                                                               
         L     RF,DBTCB                                                         
         LA    RF,TCBPSW-TCBD(RF)                                               
         L     RF,8(R1,RF)         RF=BASE REG                                  
*                                                                               
DWAT012A MVC   BYTE,1(R2)          SAVE INDEX CHR                               
         ICM   R2,3,2(R2)          R2=0000BDDD                                  
         N     R2,=X'00000FFF'                                                  
         AR    R2,RF                                                            
*                                                                               
         CLI   DMCB+5,8            QTYPE=RX                                     
         BNE   DWAT012D                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         N     R1,=X'0000000F'                                                  
         BZ    DWAT012D                                                         
         SLL   R1,2                                                             
         L     RF,DBTCB                                                         
         LA    RF,TCBPSW-TCBD(RF)                                               
         L     RF,8(R1,RF)         RF=INDEX REG                                 
         AR    R2,RF                                                            
*                                                                               
DWAT012D L     R1,=A(ADDMAP-SAVED) USE SAVED ADDR MAP                           
         AR    R1,R7                                                            
         C     R2,0(R1)            CHECK BOUNDS                                 
         BL    DWAT012F                                                         
         C     R2,4(R1)                                                         
         BH    DWAT012F                                                         
*                                                                               
         GOTO1 AHEXOUT,DMCB,(R2),23(R4),10                                      
         MVC   44(10,R4),0(R2)                                                  
*                                                                               
DWAT012F SR    R1,R1                                                            
         ICM   R1,3,HALF                                                        
         BZ    DWAT015                                                          
         XR    RF,RF                                                            
         SRL   R1,12                  0000000B                                  
         SLL   R1,2                   BASE*4                                    
         LTR   R1,R1                                                            
         BZ    DWAT012G                                                         
         L     RF,DBTCB                                                         
         LA    RF,TCBPSW-TCBD(RF)                                               
         L     RF,8(R1,RF)         RF=BASE REG                                  
*                                                                               
DWAT012G ICM   R2,3,HALF           R2=0000BDDD                                  
         N     R2,=X'00000FFF'                                                  
         AR    R2,RF                                                            
*                                                                               
         L     R1,=A(ADDMAP-SAVED) USE SAVED ADDR MAP                           
         AR    R1,R7                                                            
         C     R2,0(R1)            CHECK BOUNDS                                 
         BL    DWAT015                                                          
         C     R2,4(R1)                                                         
         BH    DWAT015                                                          
*                                                                               
         GOTO1 AHEXOUT,DMCB,(R2),77(R4),10                                      
         MVC   98(10,R4),0(R2)                                                  
*                                                                               
DWAT015  EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        BUILD USER WATCH FIELDS                            *                   
*************************************************************                   
         SPACE 1                                                                
DWAT019  LA    R4,WATCHES                                                       
         XR    R6,R6               KEEP LENGTH COUNT IN R6                      
*                                                                               
DWAT020  OC    0(16,R4),0(R4)      ANY WATCH FOUND                              
         BZ    DWAT030                                                          
*                                                                               
         MVC   0(16,R3),WATFLD3    START WITH DUMMY FIELD                       
         STCM  R5,3,2(R3)                                                       
         MVC   8(8,R3),0(R4)       SET LABEL                                    
*                                                                               
         LA    R3,16(R3)                                                        
         LA    R5,9(R5)            NEXT FIELD                                   
         LA    R6,9(R6)            UPDATE LENGTH                                
         MVC   0(78,R3),WATFLD2    DATA FIELD                                   
         STCM  R5,3,2(R3)                                                       
*                                                                               
         SR    R1,R1                                                            
         IC    R1,13(R4)                                                        
         CLI   12(R4),C'X'         HEX FIELDS ARE DOUBLE LENGTH                 
         BNE   *+8                                                              
         SLL   R1,1                SO DOUBLE IT                                 
         CHI   R1,64                                                            
         BNH   *+8                                                              
         LA    R1,64                                                            
         LA    R1,8(R1)                                                         
         STC   R1,0(R3)            SET NEW LENGTH                               
*                                                                               
         L     R2,8(R4)            GET ADDRESS                                  
         CLI   14(R4),C'A'         ABSOLUTE ADDRESS                             
         BE    DWAT024                                                          
*                                                                               
         N     R2,=X'0000FFFF'     R2 = DISPLACEMENT ONLY                       
         SR    RE,RE                                                            
         ICM   RE,1,8(R4)          BASE REG AT +8                               
         SLL   RE,2                BASE*4                                       
         L     RF,DBTCB                                                         
         LA    RF,TCBPSW-TCBD(RF)                                               
         A     R2,8(RE,RF)         RF=BASE REG                                  
*                                                                               
         CLI   14(R4),C'R'         RELATIVE                                     
         BE    DWAT024                                                          
*                                                                               
         ICM   R2,7,1(R2)          MUST BE INDIRECT                             
*                                                                               
DWAT024  L     R1,=A(ADDMAP-SAVED) USE SAVED ADDR MAP                           
         AR    R1,R7                                                            
         C     R2,0(R1)            CHECK BOUNDS                                 
         BL    DWAT025                                                          
         C     R2,4(R1)                                                         
         BH    DWAT025                                                          
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),0(R2)       MOVE IN CHARACTERS                           
*                                                                               
         CLI   12(R4),C'X'         HEX FIELD                                    
         BNE   DWAT025                                                          
*        STM   RE,R1,SAVEEF01                                                   
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+11(1),13(R4)                                                
         GOTOR HEXOUT,DMCB,(R2),8(R3)                                           
*                                                                               
DWAT025  SR    R0,R0                                                            
         IC    R0,0(R3)            NEXT FIELD                                   
         AR    R3,R0                                                            
         SHI   R0,7                                                             
         AR    R5,R0               NEXT SCREEN ADDRESS                          
         AR    R6,R0               UPDATE LINE LENGTH                           
         CHI   R6,80                                                            
         BL    DWAT030                                                          
*                                                                               
         AHI   R0,7                BACK UP TO PREVIOUS                          
         SR    R3,R0                                                            
         SHI   R3,16                                                            
         LA    R1,80                                                            
         SR    R1,R6                                                            
         AR    R5,R1                                                            
         STCM  R5,3,2(R3)                                                       
         LA    R3,16(R3)                                                        
         LA    R5,9(R5)            NEXT FIELD                                   
         LA    R6,9                UPDATE LENGTH                                
         STCM  R5,3,2(R3)                                                       
         B     DWAT025                                                          
*                                                                               
DWAT030  LA    R4,16(R4)                                                        
         LA    R1,WATCHEX                                                       
         CR    R4,R1                                                            
         BL    DWAT020                                                          
*                                                                               
DWAT090  MVC   0(3,R3),=X'000101'                                               
*                                                                               
         LA    R1,64(R8)           BUMP THROUGH TWA                             
DWAT095  SR    R0,R0                                                            
         ICM   R0,1,0(R1)                                                       
         BZ    DWATCHX                                                          
         CLC   2(2,R1),=X'0780'    MAX SCREEN ADDRESS                           
         BL    *+14                                                             
*                                                                               
         MVC   0(3,R1),=X'000101'  FORCE END OF SCREEN                          
         B     DWATCHX                                                          
*                                                                               
         AR    R1,R0                                                            
         B     DWAT095                                                          
*                                                                               
DWATCHX  XIT1                                                                   
         EJECT                                                                  
WATFLD1  DC    X'0F20000000008000',C'       '                                   
WATFLD2  DC    X'4E00000000008000'                                              
WATFLD3  DC    X'1020000000008000',C'        '                                  
         DC    80C' '                                                           
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*CTDEBWORK                                                                      
       ++INCLUDE CTDEBWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTDEB01   03/23/15'                                      
         END                                                                    
