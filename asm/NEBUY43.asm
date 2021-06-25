*          DATA SET NEBUY43    AT LEVEL 009 AS OF 12/13/10                      
*PHASE T31143A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - DISPLAY HISTORY RECORDS T31143'            
T31143   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**HIST**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
         SPACE 2                                                                
*                                                                               
         CLI   SVLMODE,PROCESS     PROCESSED THIS TIME                          
         BNE   UNIT2                                                            
         TM    MODE,FIRST                                                       
         BZ    *+10                                                             
UNIT2    XC    SVDATA(SVDATAL),SVDATA                                           
         BAS   RE,ACTED                                                         
         BAS   RE,READHIST                                                      
         BAS   RE,REPHIST                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO EDIT ACTION FIELD                                              
*                                                                               
ACTED    ST    RE,SAVEREG                                                       
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA AT END OF ACTION            
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACTM                NO                                           
         SPACE                                                                  
ACTED2   XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    ACTM                                                             
         MVI   FERN,INVERR                                                      
         MVI   FNDX,2                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),C',=,-'                              
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         SPACE                                                                  
ACTED4   MVI   FERN,DATERR                                                      
         GOTO1 VDATVAL,(R1),(0,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ACTED4A                                                          
         CLC   WORK(1),3(R1)       DOES DATE MAKE UP FIRST HALF OF FLD          
         BNE   ERROR               NO                                           
         CLC   DUB(6),ESTSTART     TEST IF DATE BEFORE EST START                
         BL    ERROR               YES                                          
         CLC   DUB(6),ESTEND       TEST IF OUTSIDE OF EST + PROF DAYS           
         BH    ERROR               NO-SAME YEAR AS EST START                    
         B     ACTED5                                                           
         SPACE                                                                  
ACTED4A  GOTO1 VDATVAL,(R1),(1,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         CLC   WORK(1),3(R1)       DOES DATE MAKE UP FIRST HALF OF FLD          
         BNE   ERROR               NO                                           
         MVC   DUB(2),ESTSTART     ESTIMATE START YEAR (INPUT=MMDD)             
         CLC   ESTSTART(2),ESTEND  TEST IF EST START/END IN SAME YEAR           
         BE    ACTED5              YES                                          
         CLC   DUB+2(4),ESTSTART+2 TEST IF INPUT MMDD LT EST ST MMDD            
         BNL   *+10                NO-SAME YEAR AS EST START                    
         MVC   DUB(2),ESTEND       YES-MUST BE YEAR OF ESTIMATE END             
         SPACE                                                                  
ACTED5   GOTO1 VDATCON,(R1),DUB,(2,DATE)                                        
         MVC   CHARDATE,DUB        SAVE YYMMDD DATE                             
         SPACE                                                                  
ACTED6   MVI   SUB,1               DEFAULT IS SUB-LINE=1                        
         CLI   WORK+1,0            TEST FOR SUB-LINE NOTATION                   
         BE    ACTEDX                                                           
         MVI   FNDX,0                                                           
         MVI   FERN,INVERR                                                      
         MVC   XTRA,SPACES                                                      
         MVC   XTRA(8),=C'SUB-LINE'                                             
         CLI   WORK+1,3                                                         
         BH    ERROR                                                            
         TM    WORK+3,X'80'        TEST IF NUMERIC                              
         BO    *+12                                                             
         MVI   FERN,NUMERR                                                      
         B     ERROR                                                            
*                                                                               
         ICM   R0,15,WORK+8                                                     
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,SUB                                                           
         B     ACTEDX                                                           
         SPACE                                                                  
*                                                                               
ACTEDX   MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         L     RE,SAVEREG                                                       
         BR    RE                                                               
*                                                                               
ACTM     MVI   FERN,MISERR                                                      
         MVC   XTRA(9),=C'UNIT DATE'                                            
         B     ERROR                                                            
         SPACE                                                                  
*                                                                               
* READ HISTORY RECORD BUILD HISTORY TABLE IF RECORD CHANGED                     
*                                                                               
READHIST NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NHRECD,R6                                                        
*                                                                               
         MVI   NHKTYPE,X'40'                                                    
         MVC   NHKPAM,AGYMED       AGENCY/MED                                   
         MVC   NHKPCLT,CLIPK       CLIENT                                       
         MVC   NHKNET,NET          NETWORK                                      
         MVC   NHKPROG,PROG        PROGRAM                                      
         MVC   NHKDATE,DATE        DATE                                         
         MVC   NHKEST,EST          ESTIMATE                                     
         MVC   NHKSUB,SUB          SUB-LINE NUMBER                              
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NHKDP,NPAKDP        DAYPART                                      
         DROP  RE                                                               
*                                                                               
         LA    R2,BUYACTH                                                       
         MVI   FERN,NOTFOUND                                                    
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         B     RDHST20                                                          
*                                                                               
RDHST10  GOTO1 AIO,DMCB,UNT+DIR+SEQ                                             
*                                                                               
RDHST20  CLC   KEY(19),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 AIO,DMCB,UNT+FILE+GET,AIOAREA2                                   
         L     R6,AIOAREA2                                                      
         CLC   NHPKG,PACK          CHECK IF PACKAGE MATCHES                     
         BNE   RDHST10                                                          
*                                                                               
         CLC   SVHISTKY,KEY        IS THIS A NEW LIST                           
         BNE   RDHST050            YES DONT SET THE POINTERS                    
         L     RE,AIOAREA2                                                      
         L     RF,SVLNTRY          RECORD DISPLACEMENT                          
         AR    RE,RF               ADD TO RECORD START                          
         ST    RE,SVLNTRY          STORE AS RECORD POINTER                      
         B     RDHSTEX                                                          
*                                                                               
RDHST050 MVC   SVHISTKY,KEY        UPDATE SAVED INFO                            
*                                                                               
* COUNT HISTORY ELEMENTS                                                        
         SR    R4,R4               ENTRY COUNTER                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'05',AIOAREA2),0                    
         CLI   12(R1),0                                                         
         BNE   RDHSTEX                                                          
         L     R3,12(R1)                                                        
         ST    R3,SVLNTRY          SAVE FIRST HISTORY ELEMENT                   
*                                                                               
         LA    R4,1(R4)                                                         
*                                                                               
RDHST100 ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'05'                                                      
         BNE   RDHST150                                                         
         LA    R4,1(R4)                                                         
         B     RDHST100                                                         
*                                                                               
RDHST150 STCM  R4,1,SVELCMTR                                                    
*                                                                               
RDHSTEX  B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* REPORT THE HISTORY RECORD                                                     
*                                                                               
REPHIST  NTR1                                                                   
         L     R6,AIOAREA2                                                      
         USING NHRECD,R6                                                        
*                                                                               
* DISPLAY ROTATION                                                              
*                                                                               
         MVC   INUDAY,SPACES                                                    
         USING NUSDRD,R3                                                        
         L     R3,12(R1)                                                        
         CLI   NHOROT,0                                                         
         BE    DISTIME                                                          
         GOTO1 VUNDAY,DMCB,NHOROT,INUDAY                                        
         OI    INUDAYH+6,X'80'                                                  
*                                                                               
* DISPLAY UNIT TIME                                                             
*                                                                               
DISTIME  MVC   INUTIME,SPACES                                                   
         GOTO1 VUNTIME,DMCB2,NHOTIME,INUTIME                                    
         OI    INUTIMEH+6,X'80'                                                 
*                                                                               
* DISPLAY PROGRAM NAME                                                          
*                                                                               
         MVC   INUPROG,SPACES                                                   
         MVC   INUPROG,NHOPRNME                                                 
         OI    INUPROGH+6,X'80'                                                 
*                                                                               
* DISPLAY LENGTH NAME                                                           
*                                                                               
         MVC   INULEN,SPACES                                                    
         EDIT  (1,NHOLEN),(3,INULEN),ALIGN=LEFT                                 
         OI    INULENH+6,X'80'                                                  
*                                                                               
* DISPLAY PRODUCT CODES                                                         
*                                                                               
         MVC   INUPROD,SPACES                                                   
         LA    R1,NNHOPRD                                                       
         LA    R2,INUPROD                                                       
*                                                                               
         CLI   NNHOPRD,X'FF'       EXTENDED BRAND ALPHA PRODS?                  
         BNE   DPRDEND                                                          
         MVC   0(3,R2),NHPROD3                                                  
         CLI   NHPROD3+3,0                                                      
         BE    DISPRD80                                                         
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'*'                                                       
         MVC   0(3,R2),NHPROD3+3                                                
         B     DISPRD80                                                         
DPRDEND  EQU   *                   END EXTENDED BRAND ALPHA PRODS               
*                                                                               
         CLI   NNHOPRD+2,0         MORE THEN 2 PRODS                            
         BE    DISPRD20                                                         
         MVC   0(3,R2),=CL3'***'                                                
         B     DISPRD80                                                         
*                                                                               
DISPRD20 LA    R0,255                                                           
         LA    RF,CLILIST                                                       
         CLC   0(1,R1),3(RF)       TEST FOR PRODUCT NUMBER                      
         BE    *+14                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   0(3,R2),0(RF)       EXTRACT PRODUCT CODE                         
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BE    DISPRD80                                                         
*                                                                               
*  POSITION OUTPUT FIELD TO HANDLE SECOND PRODUCT                               
*  AND MOVE IN THE "*" SYMBOL AS THE DELIMMETER                                 
*                                                                               
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
         B     DISPRD20                                                         
*                                                                               
DISPRD80 OI    INUPRODH+6,X'80'                                                 
*                                                                               
* DISPLAY ACTUAL COST                                                           
*                                                                               
         MVC   INUACT,SPACES                                                    
         L     R2,NHOACT           TEST FOR MINUS UNIT                          
         LTR   R2,R2                CHECK ZERO COST                             
         BNZ   DISACT20                                                         
         TM    NHSTAT,X'02'         ZERO COST INPUTTED                          
         BZ    DISACT60             NO EXIT                                     
         MVI   INUACT,C'0'                                                      
         B     DISACT60             EXIT                                        
*                                                                               
DISACT20 LR    R0,R2               SAVE COST VALUE                              
         SRDA  R2,32               PREPARE DIVIDEND                             
         D     R2,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LTR   R2,R2               TEST REMAINDER (PENNIES)                     
         BNZ   DISACT40            YES                                          
         PRINT GEN                                                              
         EDIT  (R3),(10,INUACT),ALIGN=LEFT,MINUS=YES                            
         PRINT NOGEN                                                            
         B     DISACT60                                                         
         SPACE                                                                  
DISACT40 LR    R2,R0               RESTORE COST VALUE W PENNIES                 
         EDIT  (R2),(12,INUACT),2,ALIGN=LEFT,MINUS=YES                          
*                                                                               
DISACT60 OI    INUACTH+6,X'80'                                                  
         SPACE                                                                  
*                                                                               
* DISPLAY STATUS                                                                
*                                                                               
         MVC   INUSTAT,SPACES                                                   
         MVC   INUSTAT(4),=CL4'NEW,'                                            
         MVC   INUSTAT+4(4),NHREASN                                             
         TM    NHSTAT,X'01'                                                     
         BO    *+10                                                             
         MVC   INUSTAT(8),=CL8'ORIGINAL'                                        
         OI    INUSTATH+6,X'80'                                                 
         SPACE 2                                                                
*                                                                               
* DISPLAY DATE                                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(2,NHODATE),(8,INUDTE)                              
         OI    INUDTEH+6,X'80'                                                  
         SPACE 2                                                                
*                                                                               
* DISPLAY BUYER                                                                 
*                                                                               
         MVC   INUBUYR(8),NHUSER                                                
         OI    INUBUYRH+6,X'80'                                                 
         SPACE 2                                                                
*                                                                               
*  CLEAR THE SCREEN                                                             
         LA    RE,INUDAT1                                                       
         LA    RF,INUDAT1H                                                      
         LA    R1,10                                                            
CLRSC020 MVC   0(78,RE),SPACES                                                  
         OI    6(RF),X'80'                                                      
         LA    RE,INUDAT2-INUDAT1(RE)                                           
         LA    RF,INUDAT2-INUDAT1(RF)                                           
         BCT   R1,CLRSC020                                                      
*                                                                               
*DISPLAY HISTORY RECORD DETAIL INFORMATION                                      
*                                                                               
         MVI   MORESW,NO                                                        
         OC    SVLNTRY,SVLNTRY     ANY ELEMENTS TO DISPLAY                      
         BZ    RPHST760            NO EXIT                                      
*                                                                               
         CLI   SVELCMTR,10                                                      
         BNH   RPHST050                                                         
         MVI   MORESW,YES                                                       
         LA    R3,10               NUMBER OF LINES TO OUTPUT                    
         ZIC   RE,SVELCMTR                                                      
         S     RE,=F'10'                                                        
         STCM  RE,1,SVELCMTR                                                    
         B     RPHST100                                                         
*                                                                               
RPHST050 MVI   MORESW,NO                                                        
         ZIC   R3,SVELCMTR         NUMBER OF LINES TO OUTOUT                    
         MVI   SVELCMTR,0                                                       
*                                                                               
RPHST100 LA    R2,INUDAT1                                                       
*                                                                               
         USING DLIND,R2                                                         
         USING NCHAEL,R4                                                        
         L     R4,SVLNTRY          POINTS TO FIRST ELEMENT TO DISPLAY           
         CLI   0(R4),X'05'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST POINT TO VALID ELEMENT                  
*                                                                               
RPHST200 ST    R3,SAVER3                                                        
         GOTO1 VDATCON,DMCB,(2,NCHGDATE),(8,DLINDATE)                           
         MVC   DLINBUYR,NCHGUSER   BUYER NAME                                   
         MVC   DLINREAS,NCHGREAS                                                
*                                                                               
*  COMMENT LOGIC (CAN BE A "Z" OR A "C")                                        
*                                                                               
         CLI   NCHGFCOD,C'Z'       CHECK FOR COMMENT                            
         BE    *+12                                                             
         CLI   NCHGFCOD,C'C'       CHECK FOR COMMENT                            
         BNE   RPHST220                                                         
         MVI   DLINCOMC,C'C'                                                    
         ZIC   RF,NCHGLEN                                                       
         SR    RE,RE                                                            
         LA    RE,NCHGCLEN(RE)     LENGTH OF STANDARD ELEM                      
         SR    RF,RE               LENGTH OF COMMENT                            
         BCTR  RF,0                                                             
         C     RF,=F'45'           MAX LENGTH OF FIELD                          
         BL    *+8                                                              
         LA    RF,44                                                            
         EX    RF,*+8                                                           
         B     RPHST700                                                         
         MVC   DLINCMMT(0),NCHGFLD                                              
*                                                                               
*  MISSED LOGIC                                                                 
*                                                                               
RPHST220 CLI   NCHGFCOD,C'G'       CHECK FOR MISSED                             
         BNE   RPHST300                                                         
         MVC   DLINDESC,=CL12'MADEGOOD BY '                                     
         LA    R3,NCHGFLD                                                       
         USING NUMGD,R3                                                         
         MVC   DLINPRCD,NUMGPCOD                                                
         GOTO1 VDATCON,DMCB,(2,NUMGDATE),(4,DLINBDAT)                           
*                                                                               
         CLI   NUMGSUB,2           TEST FOR SUB-LINE                            
         BL    RPHST240                                                         
         ZIC   R0,NUMGSUB                                                       
         EDIT  (R0),(3,DLINBSUB),ALIGN=LEFT                                     
         MVI   DLINBDSH,DASH                                                    
RPHST240 TM    NCHGSTAT,X'01'                                                   
         BZ    RPHST260                                                         
         MVC   DLINMORE,=CL8'*REMOVE*'                                          
         B     RPHST700                                                         
RPHST260 BAS   RE,CHKMMHST                                                      
         B     RPHST700                                                         
         DROP  R3                                                               
*                                                                               
*  MAKEGOOD LOGIC                                                               
*                                                                               
RPHST300 CLI   NCHGFCOD,C'M'       CHECK FOR MAKE-GOOD                          
         BNE   RPHST400                                                         
         MVC   DLINDESC,=CL12'MAKEGOOD FOR'                                     
         LA    R3,NCHGFLD                                                       
         USING NUMGD,R3                                                         
         MVC   DLINPRCD,NUMGPCOD                                                
         GOTO1 VDATCON,DMCB,(2,NUMGDATE),(4,DLINBDAT)                           
*                                                                               
         CLI   NUMGSUB,2           TEST FOR SUB-LINE                            
         BL    RPHST340                                                         
         ZIC   R0,NUMGSUB                                                       
         EDIT  (R0),(3,DLINBSUB),ALIGN=LEFT                                     
         MVI   DLINBDSH,DASH                                                    
RPHST340 TM    NCHGSTAT,X'01'                                                   
         BZ    RPHST360                                                         
         MVC   DLINMORE,=CL8'*REMOVE*'                                          
         B     RPHST700                                                         
RPHST360 BAS   RE,CHKMMHST                                                      
         B     RPHST700                                                         
         DROP  R3                                                               
*                                                                               
*  ALL OTHER                                                                    
*                                                                               
RPHST400 LA    RE,TRAILTAB         MOVE DESCRIPTION OUT                         
*                                                                               
RPHST420 CLI   0(RE),X'FF'                                                      
         BNE   *+6                 INVALID TYPE CODE IN ELEMENT                 
         DC    H'0'                                                             
         CLC   NCHGFCOD,0(RE)                                                   
         BE    RPHST440                                                         
         LA    RE,30(RE)           NEXT ENTRY                                   
         B     RPHST420                                                         
*                                                                               
RPHST440 MVC   DLINTRL(29),1(RE)   MOVE OUT TRAIL                               
*                                                                               
*  MOVE OUT OLD VALUE                                                           
*                                                                               
RPHST500 LA    RE,TRAILTAB         MOVE DESCRIPTION OUT                         
         SR    RF,RF                                                            
*                                                                               
RPHST520 CLI   0(RE),X'FF'                                                      
         BNE   *+6                 INVALID TYPE CODE IN ELEMENT                 
         DC    H'0'                                                             
         CLC   NCHGFCOD,0(RE)                                                   
         BE    RPHST540                                                         
         LA    RE,30(RE)           NEXT ENTRY                                   
         LA    RF,8(RF)            BUMP UP TOP NEXT ADDRESS                     
         B     RPHST520                                                         
*                                                                               
RPHST540 B     DISVALTB(RF)        BRANCH TO ROUTINE                            
*                                                                               
DISVALTB BAS   RE,DISOPRD          DISPLAY PRODUCT                              
         B     RPHST700                                                         
         BAS   RE,DISOLEN          DISPLAY LENGTH                               
         B     RPHST700                                                         
         BAS   RE,DISOACT          DISPLAY ACTUAL COST                          
         B     RPHST700                                                         
         BAS   RE,DISODAT          DISPLAY DATE                                 
         B     RPHST700                                                         
         BAS   RE,DISOTME          DISPLAY TIME                                 
         B     RPHST700                                                         
         BAS   RE,DISOPRG          DISPLAY PROGRAM NAME                         
         B     RPHST700                                                         
         BAS   RE,DISOROT          DISPLAY ROTATION                             
         B     RPHST700                                                         
         BAS   RE,DISOPRE          DISPLAY PRE-EMPT                             
         B     RPHST700                                                         
*                                                                               
RPHST700 XC    SVLNTRY,SVLNTRY                                                  
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'05'                                                      
         BE    RPHST720                                                         
         MVI   MORESW,NO                                                        
         B     RPHST760                                                         
RPHST720 ST    R4,SVLNTRY          SAVE NEXT ELEMNT ADDRESS                     
         OI    6(R2),X'80'         DISPLAY LINE                                 
         LA    R2,INUDAT2H-INUDAT1H(R2)  GO TO NEXT LINE                        
         L     R3,SAVER3                                                        
         BCT   R3,RPHST200                                                      
*        SPACE                                                                  
*  END OF SCREEN LOGIC                                                          
*                                                                               
RPHST760 MVC   BUYMSG(L'INFOMSG),INFOMSG                                        
         CLI   MORESW,YES          TEST FOR MORE TO COME                        
         BNE   *+14                                                             
         MVC   BUYMSG+L'INFOMSG+1(14),=C'(MORE TO COME)'                        
         B     *+10                                                             
         MVC   BUYMSG+L'INFOMSG+1(20),=C'- ENTER NEXT REQUEST'                  
         CLI   MORESW,YES          TEST FOR END OF DISPLAY                      
         BE    RPHST780            NO                                           
*                                                                               
         XC    SVDATA(SVDATAL),SVDATA CLEAR SAVE AREA                           
         MVI   SVLMODE,EOF         FORCE FIRST TIME NEXT                        
         B     RPHSTEX                                                          
         SPACE                                                                  
RPHST780 OI    BUYACTH+6,X'01'     MODIFIED NEXT TIME                           
         MVI   SVLMODE,PROCESS     PROCESSED THIS TIME                          
         L     RE,AIOAREA2                                                      
         L     RF,SVLNTRY                                                       
         SR    RF,RE                                                            
         ST    RF,SVLNTRY          SAVE ABSOLUTE RECORD DISPLACEMENT            
         SPACE                                                                  
RPHSTEX  LA    R2,BUYACTH          MODULE EXIT                                  
         ST    R2,FADDR                                                         
         NI    MODE,X'FF'-FIRST-DISPLAY                                         
         B     EXXMOD                                                           
*                                                                               
TRAILTAB DC    CL30'BBRAND CHANGE                 '                             
         DC    CL30'LLENGTH CHANGE                '                             
         DC    CL30'AACTUAL COST CHANGE           '                             
         DC    CL30'DBUY DATE CHANGE              '                             
         DC    CL30'TTIME CHANGE                  '                             
         DC    CL30'NPROGRAM NAME CHANGE          '                             
         DC    CL30'RROTATION CHANGE              '                             
         DC    CL30'PPREEMPT                      '                             
         DS    X'FF'                                                            
**************************                                                      
*                                                                               
* IF MAD OR MAKEGOOD READ REFERENCED HISTORY RECORD                             
* TO SEE IF CHANGES WERE MADE TO IT IF SO MOVE "MORE'                           
* INDICATOR OUT TO THE SCREEN                                                   
*                                                                               
CHKMMHST NTR1                                                                   
         LA    R3,NCHGFLD                                                       
         USING NUMGD,R3                                                         
*                                                                               
*  BUILD HISTORY KEY                                                            
*                                                                               
*                                                                               
         LA    R6,KEY                                                           
         MVI   NHKTYPE,X'40'                                                    
         MVC   NHKPAM,AGYMED       AGENCY/MED                                   
         MVC   NHKPCLT,CLIPK       CLIENT                                       
         MVC   NHKNET,NET          NETWORK                                      
         MVC   NHKPROG,NUMGPCOD    PROGRAM                                      
         MVC   NHKDATE,NUMGDATE    DATE                                         
         MVC   NHKEST,EST          ESTIMATE                                     
         MVC   NHKSUB,NUMGSUB      SUB-LINE NUMBER                              
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NHKDP,NPAKDP        DAYPART                                      
         DROP  RE                                                               
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(19),KEYSAVE                                                  
         BE    CHKMM020                                                         
         MVI   FERN,NOTFOUND                                                    
         LA    R2,BUYACTH                                                       
         B     ERROR                                                            
*                                                                               
CHKMM020 GOTO1 AIO,DMCB,UNT+FILE+GET,AIOAREA3                                   
*                                                                               
* SEE IF HISTORY ELEMENTS EXIST IF SO MOVE MORE INDICATOR OUT                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'05',AIOAREA3),0                    
         CLI   12(R1),0                                                         
         BNE   CHKMMHEX                                                         
         L     R3,12(R1)                                                        
*                                                                               
* THE FIRST 05 ELEMENT IS THE MISSED/MAKEGOOD PAIR                              
* BUT IF A SECOND 05 ELEMENT EXISTS WE KNOW REAL                                
* ACTIVITY WAS DONE ON THE OTHER RECORD                                         
*                                                                               
         ZIC   RE,1(R3)                                                         
         AR    RE,R3                                                            
         CLI   0(RE),X'05'                                                      
         BNE   CHKMMHEX                                                         
         MVC   DLINMORE,=CL8'**MORE**'                                          
*                                                                               
CHKMMHEX B     EXXMOD              SAVE FIRST HISTORY ELEMENT                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY ROTATION                                                              
*                                                                               
DISOROT  NTR1                                                                   
         USING NCHAEL,R4                                                        
         CLI   NCHGFLD,0                                                        
         BE    DISOROTX                                                         
         GOTO1 VUNDAY,DMCB,NCHGFLD,DLINOVAL                                     
DISOROTX B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* DISPLAY UNIT TIME                                                             
*                                                                               
DISOTME  NTR1                                                                   
         USING NCHAEL,R4                                                        
         GOTO1 VUNTIME,DMCB2,NCHGFLD,DLINOVAL                                   
DISOTIMX B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* DISPLAY PROGRAM NAME                                                          
*                                                                               
DISOPRG  NTR1                                                                   
         USING NCHAEL,R4                                                        
         MVC   DLINOVAL,NCHGFLD                                                 
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* DISPLAY LENGTH                                                                
*                                                                               
DISOLEN  NTR1                                                                   
         USING NCHAEL,R4                                                        
         EDIT  (1,NCHGFLD),(3,DLINOVAL),ALIGN=LEFT                              
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* DISPLAY PRODUCT CODES                                                         
*                                                                               
DISOPRD  NTR1                                                                   
         USING NCHAEL,R4                                                        
         LA    R1,NCHGFLD                                                       
         LA    R2,DLINOVAL                                                      
*                                                                               
         TM    NCHGSTAT,X'04'      ALPHA PRODS                                  
         BZ    DISALPHX                                                         
         CLI   NCHGFLD+6,X'40'     MORE THAN 2 PRODS                            
         BH    DISOPR10                                                         
         MVC   0(3,R2),0(R1)                                                    
         CLI   3(R1),X'40'         MORE THAN 1 PROD                             
         BNH   DISOPR80                                                         
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'40'         3 CHAR PROD?                                 
         BNH   *+8                                                              
         LA    R2,1(R2)            YES                                          
         MVI   0(R2),C'*'                                                       
         MVC   1(3,R2),3(R1)                                                    
         B     DISOPR80                                                         
DISALPHX EQU   *                                                                
*                                                                               
         CLI   NCHGFLD+2,0         MORE THEN 2 PRODS                            
         BE    DISOPR20                                                         
DISOPR10 MVC   0(3,R2),=CL3'***'                                                
         B     DISOPR80                                                         
*                                                                               
DISOPR20 LA    R0,255                                                           
         LA    RF,CLILIST                                                       
         CLC   0(1,R1),3(RF)       TEST FOR PRODUCT NUMBER                      
         BE    *+14                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   0(3,R2),0(RF)       EXTRACT PRODUCT CODE                         
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BE    DISOPR80                                                         
*                                                                               
*  POSITION OUTPUT FIELD TO HANDLE SECOND PRODUCT                               
*  AND MOVE IN THE "*" SYMBOL AS THE DELIMMETER                                 
*                                                                               
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
         B     DISOPR20                                                         
*                                                                               
DISOPR80 B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* DISPLAY ACTUAL COST                                                           
*                                                                               
DISOACT  NTR1                                                                   
         USING NCHAEL,R4                                                        
         ICM   RE,15,NCHGFLD                                                    
         LTR   RE,RE                CHECK ZERO COST                             
         BNZ   DISOAC20                                                         
         TM    NCHGSTAT,X'02'       ZERO COST INPUTTED                          
         BZ    DISOAC60             NO EXIT                                     
         MVI   DLINOVAL,C'0'                                                    
         B     DISOAC60             EXIT                                        
DISOAC20 LR    R0,RE                                                            
         SRDA  RE,32               PREPARE DIVIDEND                             
         D     RE,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LTR   RE,RE               TEST REMAINDER (PENNIES)                     
         BNZ   DISOAC40            YES                                          
         LR    R0,RF                                                            
         EDIT  (R0),(11,DLINOVAL),ALIGN=LEFT,MINUS=YES                          
         B     DISOAC60                                                         
         SPACE                                                                  
DISOAC40 EDIT  (R0),(12,DLINOVAL),2,ALIGN=LEFT,MINUS=YES                        
*                                                                               
DISOAC60 B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* DISPLAY DATE                                                                  
*                                                                               
DISODAT  NTR1                                                                   
         USING NCHAEL,R4                                                        
         GOTO1 VDATCON,DMCB,(2,NCHGFLD),(8,DLINOVAL)                            
*                                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* DISPLAY PRE-EMPT                                                              
*                                                                               
DISOPRE  NTR1                                                                   
         B     EXXMOD                                                           
         EJECT                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
INFOMSG  DC    C'** HISTORY INFO DISPLAYED'                                     
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
       ++INCLUDE NETDEMOT                                                       
* NETDEMOE (DSECT COVERING NETWORK DEMO BLOCK SUPPORTS 50 DEMOS)                
*                                                                               
       ++INCLUDE NETDEMOP                                                       
         EJECT                                                                  
* INFO UNIT SCREEN                                                              
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYE2D                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    0D                                                               
SVLNTRY  DS    F                   ADDRESS TO NEXT ELEMENT TO DISPLAY           
SVLMODE  DS    X                   LAST TIME PROCESS MODE                       
SVELCMTR DS    X                   NUMBER OF HISTORY ELEMENTS                   
SVHISTKY DS    XL20                KEY OF HISTORY RECORD                        
*                                                                               
SVVALS   DS    0C                  LAST TIME CONTROL VALUES                     
SVPROD   DS    X                                                                
SVSTDATE DS    XL2                                                              
SVENDATE DS    XL2                                                              
SVDP     DS    C                                                                
SVLOCK   DS    C                                                                
SVLOPT   DS    C                   LAST TIME REPORT OPTION (D,P)                
SVVALSLN EQU   *-SVVALS                                                         
*                                                                               
SVDATAL  EQU   *-SVDATA                                                         
*                                                                               
SVNBLOCK DS    XL(NEBLOCKL)                                                     
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
MORESW   DS    C                                                                
*                                                                               
*--ACCUMULATORS FOR TYPE T REPORT                                               
SAVER3   DS    F                   SAVE R3                                      
*                                                                               
THISVALS DS    0CL(SVVALSLN)       THIS TIME CONTROL VALUES                     
PROD     DS    X                                                                
STDATE   DS    XL2                                                              
ENDATE   DS    XL2                                                              
DAYPART  DS    C                                                                
LOCK     DS    C                                                                
REPOPT   DS    C                                                                
MAXIOCTR DS    H                                                                
*                                                                               
DATE     DS    XL2                 DATE EXTRACTED FROM ACTION FIELD             
CHARDATE DS    CL6                 DATE FROM ACTION FIELD - YYMMDD              
SUB      DS    X                   SUBLINE EXTRACTED FROM ACTION FIELD          
*                                                                               
ENTRIES  DS    X                                                                
*                                                                               
         DS    0D                                                               
BLOCK    DS    CL256                                                            
         SPACE 2                                                                
* DSECT TO COVER SCREEN LINE                                                    
*                                                                               
DLIND    DSECT                                                                  
DLINDATE DS    CL8                                                              
         DS    CL3                                                              
DLINREAS DS    CL4                                                              
         DS    CL3                 SPARE                                        
DLINTRL  DS    CL29                                                             
         DS    CL3                                                              
DLINOVAL DS    CL17                                                             
         DS    CL3                                                              
DLINBUYR DS    CL8                                                              
         ORG   DLINTRL             COMMENT LAYOUT                               
DLINCOMC DS    CL1                                                              
         DS    CL3                                                              
DLINCMMT DS    CL45                                                             
         ORG   DLINTRL             MISSED/MAKEGOOOD LAYOUT                      
DLINDESC DS    CL12                                                             
         DS    CL2                                                              
DLINPRCD DS    CL6                                                              
         DS    CL2                                                              
DLINBDAT DS    CL6                                                              
DLINBDSH DS    CL1                                                              
DLINBSUB DS    CL3                                                              
         DS    CL2                                                              
DLINMORE DS    CL8                                                              
         SPACE 2                                                                
*                                                                               
*  EQUATES                                                                      
*                                                                               
PROCESS  EQU   X'01'               PROCESSED DISPLAY LAST TIME                  
EOF      EQU   X'02'               FINISHED DISPLAY LAST TIME                   
         SPACE 2                                                                
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009NEBUY43   12/13/10'                                      
         END                                                                    
